/*
 * The MIT License
 *
 * Copyright (c) 2018 Fulcrum Genomics
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 */

package com.fulcrumgenomics.commons.async

import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}


/** A trait that asynchronous classes can use to build one or more daemon threads, or similarly [[Runnable]]s.
  *
  * The daemon threads are built with the `buildThread()` method, and set as daemons, but are not started. They will
  * execute the same run method, which is wrapped to propagate exceptions from each underlying asynchronous thread.  The
  * thread will execute the `run()` method in a try block. If an exception occurs, `throwable` will be set if not
  * already defined, and then `uponException()` will be executed.  Finally, the `uponFinally()` method will be executed.
  * The thread is set as a daemon but not started.
  *
  * The `checkAndRethrow` method should be used to check if any asynchronous thread has encountered an exception, and if
  * so, rethrow the exception as an [[Error]] or [[RuntimeException]].
  *
  * The `tryAndModifyInterruptedException` method should be used on a block of code that blocks, and will throw a
  * [[RuntimeException]] if the interrupted.
  *
  * Two common uses cases for classes extending this trait are:
  *
  * 1. Classes that have one asynchronous thread, such as [[AsyncSink]] or [[AsyncIterator]], that performs all work in
  * a separate thread.  This may be useful if the work is not to be parallelized, but simply asynchronous.
  *
  * 2. Classes that have multiple asynchronous threads, such as [[AsyncMultiWriter]], that perform work across multiple
  * threads.  This may be useful if the work can be parallelized.
  * */
private[async] trait AsyncThreadBuilder {
  protected val threadsDone: AtomicBoolean = new AtomicBoolean(false)
  private[async] val throwable: AtomicReference[Option[Throwable]] = new AtomicReference[Option[Throwable]](None)

  /** The run method to execute in the asynchronous threads. */
  protected def run(): Unit

  /** Returns true if all the threads have completed their work, false otherwise. */
  protected def allThreadsDone: Boolean = this.threadsDone.get

  /** Use this method to indicate that all threads built have completed their work.  There still may be work remaining
    * in the main thread.  Returns true if the threads were already done, false otherwise. */
  protected def setThreadsDone(): Boolean = this.threadsDone.getAndSet(true)

  /** The method to execute if an exception occurs in the asynchronous thread. */
  protected def uponException(): Unit = Unit

  /** The method to execute upon successfully execution of the run method or an exception occurs. */
  protected def uponFinally(): Unit = Unit

  /** Builds a [[Runnable]] that will execute the `run` method. */
  protected def buildRunnable(): Runnable = {
    val outer = this
    new Runnable {
      override def run(): Unit = {
        try {
          outer.run()
        } catch {
          case thr: Throwable =>
            outer.throwable.compareAndSet(None, Some(thr))
            outer.uponException()
        } finally {
          outer.uponFinally()
        }
      }
    }
  }

  /** Builds an asynchronous daemon thread to execute the `run` method.  */
  protected def buildThread(name: String): Thread = {
    // NB: implicit conversion to Runnable not supported in scala 2.11
    val runnable = buildRunnable()
    val t = new Thread(runnable, name)
    t.setDaemon(true)
    t
  }

  /** Checks to see if an exception has been raised by an asynchronous thread and if so rethrows it as an [[Error]]
    * or [[RuntimeException]] as appropriate.  Use this method before code that assumes the threads have not encountered
    * an exception.
    */
  protected final def checkAndRethrow(): Unit = {
    this.throwable.getAndSet(None).foreach { thr =>
      this.setThreadsDone()// Ensure no further attempts to run
      thr match {
        case e: Error             => throw e
        case ex: RuntimeException => throw ex
        case t                    => throw new RuntimeException(t)
      }
    }
  }

  /** Executes the given block of code.  If an [[InterruptedException]] is thrown, throws a [[RuntimeException]]
    * with the given message.  Use this method for blocking code that when interrupted should not be recoverable.
    *
    * @param message the message to use if an [[InterruptedException]] is thrown by the block of code
    * @param f the block of code to execute
    */
  protected def tryAndModifyInterruptedException[T](message: String)(f: => T): T = {
    try { f } catch {
      case ie: InterruptedException => throw new RuntimeException(message, ie)
    }
  }
}