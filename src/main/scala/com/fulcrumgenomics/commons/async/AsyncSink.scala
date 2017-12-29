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
import java.util.concurrent._
import java.util.concurrent.atomic.AtomicBoolean

object AsyncSink {
  /** Builds an [[AsyncSink]] and starts it.
    *
    * @param sink the method to invoke to process an object of type [[T]].
    * @param bufferSize the number of elements to buffer before blocking when processing the elements, or None if
    *                   unbounded.
    * @param source the optional source to close when this sink is closed.
    * @tparam T the type of object to process
    */
  def apply[T](sink: T => Unit, bufferSize: Option[Int] = None, source: Option[{ def close(): Unit }] = None): AsyncSink[T] = {
    new AsyncSink[T](sink, bufferSize, source).start()
  }
}



/** Asynchronous wrapper for applying a `sink` method to items of type [T].  Items can be added to this sink (with
  * `add()`) until no more items will be added (indicated by calling the `close()` method), ensuring all items added to
  * the sink will have the `sink` method applied to them.
  *
  * NOTE: Any exception thrown by the [[sink]] method will be propagated back to the caller
  * during the next available call to [[add()]] or [[close()]]. After the exception
  * has been thrown to the caller, it is not safe to attempt further operations on the instance.
  *
  * NOTE: [[add()]] or [[close()]] are not thread-safe, so there must be only one thread that calls them.
  *
  * @param sink the method to invoke to process an object of type [[T]].
  * @param bufferSize the number of elements to buffer before blocking when processing the elements, or None if
  *                   unbounded.
  * @param source the optional source to close when this sink is closed.
  * @tparam T the type of object to process
  */
class AsyncSink[T](val sink: T => Unit, bufferSize: Option[Int] = None, val source: Option[{ def close(): Unit }] = None)
  extends Closeable with AsyncRunnable {
  bufferSize.foreach(size => require(size > 0, s"bufferSize must be greater than zero when given, found $size"))

  private val isClosed: AtomicBoolean = new AtomicBoolean(false)

  private val queue: BlockingQueue[T] = bufferSize match {
    case Some(size) => new ArrayBlockingQueue[T](size)
    case None       => new LinkedBlockingDeque[T]()
  }

  /** Adds the item to the queue to be processed by [[sink]].
    *
    * @param item the item to queue
    */
  def add(item: T): Unit = {
    if (this.isClosed.get()) throw new RuntimeException("Attempt to add record to closed sink.")
    checkAndRaise()
    tryAndModifyInterruptedException("Interrupted queueing item.") { this.queue.put(item) }
    checkAndRaise()
  }

  /** Attempts to finish draining the queue and then calls `close()` on the source to allow implementation
    * to do any one time clean up.
    */
  def close(): Unit = {
    require(this.started, "Attempting to close a sink that hasn't been started.")
    checkAndRaise()
    if (this.isClosed.compareAndSet(false, true)) {
      // Wait for the execute method to complete
      awaitDone()
      // The queue should be empty but if it's not, we'll drain it here to protect against any lost data.
      // There's no need to timeout on poll because take is called only when queue is not empty and
      // at this point the thread is definitely dead and no-one is removing items from the queue.
      // The item pulled will never be null (same reasoning).
      while (!this.queue.isEmpty) {
        sink(this.queue.take())
      }
      this.source.foreach(_.close())
      checkAndRaise()
    }
  }

  protected def execute(): Unit = {
    // The order of the two conditions is important, because we want to make sure that emptiness status of the
    // queue does not change after we have evaluated isClosed as it is now (isClosed checked before
    // queue.isEmpty), the two operations are effectively atomic if isClosed returns true
    while (!this.isClosed.get() || !this.queue.isEmpty) {
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