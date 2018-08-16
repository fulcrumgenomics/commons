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
import java.util.concurrent.atomic.AtomicBoolean
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, LinkedBlockingDeque}

import com.fulcrumgenomics.commons.io.Writer

import scala.concurrent.forkjoin.ForkJoinPool


/** Asynchronously writes to multiple writers.
  *
  * Creates `parallelism` threads to asynchronously write items to be written to a single writer.  Each writer has a
  * queue of items to be written, which is retrieved by a single thread.  The retrieved queue of items is written to
  * the writer until no more items are in that queue.  The thread then retrieves another non-empty queue until no
  * more items are to be written (i.e. the `close()` method is called).  The underlying writers are closed when this
  * writer is closed.
  *
  * Each thread will wait (sleep) a certain amount of time after unsuccessfully attempting to retrieve a non-empty
  * queue of items.  The amount of time changes dynamically.  After each unsuccessful attempt, the wait time is
  * increased by `incrementWaitMillis` up to a maximum time of `maximumWaitMillis`.  After a successful attempt, the
  * wait time is scaled down by the `decreaseWaitFactor` down to the minimum time of `minimumWaitMillis`.  The wait
  * time will initially set to `minimumWaitMillis`.
  *
  * NOTE: Any exception thrown by a writer will be propagated back to the caller
  * during the next available call to [[write()]] or [[close()]]. After the exception
  * has been thrown to the caller, it is not safe to attempt further operations on the instance.
  *
  * NOTE: [[write()]] or [[close()]] are not thread-safe, so only there must be only one thread that calls them.
  *
  * @param writers the writers
  * @param parallelism the number of threads in which to asynchronously write
  * @param bufferSize the number of elements per writer to buffer before blocking when writing the elements, or None
  *                   if unbounded
  * @param minimumWaitMillis the minimum time to sleep after unsuccessfully retrieving a non-empty queue of items
  * @param maximumWaitMillis the maximum time to sleep after unsuccessfully retrieving a non-empty queue of items
  * @param incrementWaitMillis the amount of time to increase the sleep time after unsuccessfully retrieving a
  *                            non-empty queue of items
  * @param decreaseWaitFactor the factor to scale the sleep time (0 < decreasePollFactor < 1) after successfully
  *                           retrieving a non-empty queue of items
  * @tparam T the type of items to write.
  */
class AsyncMultiWriter[T](writers: Traversable[Writer[T]],
                          parallelism: Int,
                          bufferSize: Option[Int] = None,
                          minimumWaitMillis: Int = 50,
                          maximumWaitMillis: Int = 1000,
                          incrementWaitMillis: Int = 50,
                          decreaseWaitFactor: Double = 0.5
                         ) extends Closeable with AsyncThreadBuilder with Writer[(T, Int)]{
  require(writers.nonEmpty, "must supply at least one writer")
  bufferSize.foreach(size => require(size > 0, s"bufferSize must be greater than zero when given, found $size"))
  require(parallelism > 0, "parallelism must be greater than zero")
  require(decreaseWaitFactor > 0 && decreaseWaitFactor < 1, "decreasePollFactor must be between zero and one exclusive")
  require(minimumWaitMillis <= maximumWaitMillis, "maximumPollMillis < minimumPollMillis")

  private val _writers: Array[Writer[T]] = writers.toArray
  private val queues: Array[BlockingQueue[T]] = _writers.map { _ =>
    bufferSize match {
      case Some(size) => new ArrayBlockingQueue[T](size)
      case None       => new LinkedBlockingDeque[T]()
    }
  }
  private val locks: Array[AtomicBoolean] = _writers.map { _ => new AtomicBoolean(false) }
  private val threads: Seq[Thread] = Range.inclusive(1, parallelism).map { i =>
    buildThread(getClass.getSimpleName + "-thread-" + i)
  }

  // Start up the threads
  this.threads.foreach(_.start())

  protected def run(): Unit = {
    var allDone: Boolean = false
    var sleepMillis: Int = minimumWaitMillis
    while (!allDone) {
      // find a non-empty queue
      this.queues.indexWhere { queue => !queue.isEmpty } match {
        case -1 =>
          allDone = this.allThreadsDone // only exit the loop if all queues are empty and no more items will be added
          Thread.sleep(sleepMillis) // sleep, nothing to do
          sleepMillis = Math.min(maximumWaitMillis, sleepMillis + incrementWaitMillis) // backoff next time
        case i  =>
          val lock  = this.locks(i)
          if (!lock.getAndSet(true)) { // not previously locked, so this queue is ours now
            try {
              val queue = this.queues(i)
              // write items until no more in the queue
              val writer = this._writers(i)
              while (!queue.isEmpty) {
                // NB: we could take from the queue but write it (ex. an IOException is thrown).  Unfortunately, there's
                // no way to recover the taken item.
                writer.write(queue.take())
              }
              sleepMillis = Math.max(minimumWaitMillis, (sleepMillis * decreaseWaitFactor).toInt) // don't sleep as much next time
            }
            finally {
              // in the case of any exception, unlock the queue
              lock.set(false)
            }
          }
      }
    }
  }

  def write(itemAndIndex: (T, Int)): Unit = {
    val (item, writerIndex) = itemAndIndex
    if (this.allThreadsDone) throw new RuntimeException("Attempt to add item to a closed writer")
    checkAndRethrow()
    tryAndModifyInterruptedException("Interrupted queueing item.") { this.queues(writerIndex).put(item) }
    checkAndRethrow()
  }

  def close(): Unit = {
    checkAndRethrow()
    if (!this.setThreadsDone()) {
      this.threads.foreach { thread =>
        tryAndModifyInterruptedException(s"Interrupted waiting on thread '${thread.getName}'.") { thread.join() }
      }

      // The queues should be empty but if they are not, we'll drain it here to be more robust to data loss (still
      // could occur if the item was taken from the queue but not written).
      this.queues.zipWithIndex.filterNot(_._1.isEmpty).foreach { case (queue, index) =>
        val writer = this._writers(index)
        while (!queue.isEmpty) {
          writer.write(queue.take())
        }
      }

      // close them asynchronously, since some writers may take a **long** time to close.
      import com.fulcrumgenomics.commons.CommonsDef.ParSupport
      val pool = new ForkJoinPool(parallelism)
      this._writers.toSeq.parWith(pool=pool).foreach(_.close())

      checkAndRethrow()
    }
  }
}