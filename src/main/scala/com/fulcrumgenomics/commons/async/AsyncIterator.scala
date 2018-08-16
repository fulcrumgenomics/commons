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

import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, LinkedBlockingDeque}

object AsyncIterator {
  /** A counter used for thread naming in [[AsyncIterator]] */
  private val threadsCreated = new AtomicInteger(0)
}

/** An asynchronous wrapper for an [[Iterator]].  A separate thread will be created to consume the source iterator.
  * Will buffer up to [[bufferSize]] elements before the blocking when consuming the source.
  *
  * @param source the source iterator to consume
  * @param bufferSize the maximum number of elements to buffer before the blocking when consuming the source, or None
  *                   if unbounded.
  * @tparam T the type of object to consume
  */
class AsyncIterator[T](private val source: Iterator[T], bufferSize: Option[Int] = None) extends Iterator[T] with AsyncThreadBuilder {
  bufferSize.foreach(size => require(size > 0, s"bufferSize must be greater than zero when given, found $size"))

  private val queue: BlockingQueue[T] = bufferSize match {
    case Some(size) => new ArrayBlockingQueue[T](size)
    case None       => new LinkedBlockingDeque[T]()
  }
  private val thread = buildThread(this.getClass.getSimpleName + AsyncIterator.threadsCreated.incrementAndGet)

  // Start the thread after creating the queue
  this.thread.start()

  protected def run(): Unit = this.source.foreach(this.queue.put)

  override protected def uponFinally(): Unit = this.setThreadsDone()

  /** Returns true if there exists more elements, false otherwise */
  def hasNext(): Boolean = {
    checkAndRethrow()

    // If the queue is empty but the underlying thread is not done, then there may or may not be new
    // items that will be put in the queue.
    while (this.queue.isEmpty && !this.allThreadsDone) {
      Thread.sleep(50)
      checkAndRethrow() // check if there was any exception in the underlying thread
    }

    // At this point, either there are items in the queue, or the underlying thread has consumed the source
    if (!this.queue.isEmpty) true
    else { // the queue is empty, and the source has been consumed
      require(this.allThreadsDone, "Bug: iterator was not done when the thread was joined.")
      tryAndModifyInterruptedException("Interrupted waiting on iterator thread to join.") { this.thread.join() }
      checkAndRethrow()
      !this.queue.isEmpty
    }
  }

  /** Gets the next item. */
  def next(): T = {
    checkAndRethrow()
    if (this.hasNext()) {
      val item = tryAndModifyInterruptedException("Interrupted waiting on taking from the queue.") { this.queue.take() }
      checkAndRethrow()
      item
    } else {
      throw new NoSuchElementException("Calling next() when hasNext() is false.")
    }
  }
}