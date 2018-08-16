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

import java.io.Closeable
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, LinkedBlockingDeque, TimeUnit}


object AsyncSink {
  /** A counter used for thread naming in [[AsyncSink]] */
  private val threadsCreated = new AtomicInteger(0)
}

/** An asynchronous wrapper for processing items of type [[T]] and process them in a separate thread using
  * the provided `sink` method.
  *
  * NOTE: Any exception thrown by the [[sink]] method will be propagated back to the caller
  * during the next available call to [[add()]] or [[close()]]. After the exception
  * has been thrown to the caller, it is not safe to attempt further operations on the instance.
  *
  * NOTE: [[add()]] or [[close()]] are not thread-safe, so only there must be only one thread that calls them.
  *
  * @param sink the method to invoke to process an object of type [[T]].
  * @param bufferSize the number of elements to buffer before blocking when processing the elements, or None if
  *                   unbounded.
  * @param source the optional source to close when this sink is closed.
  * @tparam T the type of object to process
  */
class AsyncSink[T](val sink: T => Unit, bufferSize: Option[Int] = None, val source: Option[{ def close(): Unit }] = None)
  extends Closeable with AsyncThreadBuilder {
  bufferSize.foreach(size => require(size > 0, s"bufferSize must be greater than zero when given, found $size"))
  private val queue: BlockingQueue[T] = bufferSize match {
    case Some(size) => new ArrayBlockingQueue[T](size)
    case None       => new LinkedBlockingDeque[T]()
  }
  private val thread = buildThread(this.getClass.getSimpleName + AsyncSink.threadsCreated.incrementAndGet)

  // Start the thread after creating the queue
  this.thread.start()

  /** Adds the item to the queue to be processed by [[sink]].
    *
    * @param item the item to queue
    */
  def add(item: T): Unit = {
    if (this.allThreadsDone) throw new RuntimeException("Attempt to add record to closed sink.")
    checkAndRethrow()
    tryAndModifyInterruptedException("Interrupted queueing item.") { this.queue.put(item) }
    checkAndRethrow()
  }

  /** Attempts to finish draining the queue and then calls `close()` on the source to allow implementation
    * to do any one time clean up.
    */
  def close(): Unit = {
    checkAndRethrow()
    if (!this.setThreadsDone()) {
      tryAndModifyInterruptedException("Interrupted waiting on sink thread.") { this.thread.join() }
      // The queue should be empty but if it's not, we'll drain it here to protect against any lost data.
      // There's no need to timeout on poll because take is called only when queue is not empty and
      // at this point the thread is definitely dead and no-one is removing items from the queue.
      // The item pulled will never be null (same reasoning).
      while (!this.queue.isEmpty) {
        sink(this.queue.take())
      }
      this.source.foreach(_.close())
      checkAndRethrow()
    }
  }

  protected def run(): Unit = {
    // The order of the two conditions is important, because we want to make sure that emptiness status of the
    // queue does not change after we have evaluated isClosed as it is now (isClosed checked before
    // queue.isEmpty), the two operations are effectively atomic if isClosed returns true
    while (!this.allThreadsDone || !this.queue.isEmpty) {
      val item = this.queue.poll(50, TimeUnit.MILLISECONDS)
      if (item != null) this.sink(item)
    }
  }

  override protected def uponException(): Unit = {
    // In case the sink was blocking on a full queue before ex has been set, clear the queue
    // so that the sink will no longer be blocked so that it can see the exception.
    this.queue.clear()
  }
}