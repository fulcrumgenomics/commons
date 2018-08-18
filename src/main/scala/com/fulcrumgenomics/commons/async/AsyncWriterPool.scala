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
import java.util.concurrent.atomic.{AtomicBoolean, AtomicReference}

import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.io.Writer

/**
  * A shared thread pool for making asynchronous [[Writer]]s without allocating a thread per writer.
  * The pool is created with a fixed number of threads which are used to handle write operations for
  * all writers associated to the pool.
  *
  * Writers are added to the pool using the [[pool()]] method, which returns a [[PooledWriter]] that
  * can be written to in place of the original [[Writer]].  [[Writer]]s of different types can be mixed
  * within the same pool, and each writer can specify either an unbounded queue or a fixed queue of
  * independent size when being pooled.
  *
  * [[Writer]] can be added to the pool at any time (except aftering calling [[close()]] on the pool.
  * Writers can be closed individually (and removed from the pool) using either their [[PooledWriter.close()]]
  * method which runs synchronously on the calling thread, or by using [[PooledWriter.closeAsync()]]
  * which will execute the close asynchronously and return a [[Future]] upon which `get()` must be called
  * to re-throw any exceptions that were thrown during the close operation.
  *
  * The entire pool can be closed be calling [[close()]] on the pool.  This will asynchronously
  * close all open writers, shutdown the internal thread pool, and rethrow the first exception (if
  * any) encountered when closing writers.
  *
  * @param threads the number of threads to use to write items.
  */
class AsyncWriterPool(threads: Int) extends Closeable {
  require(threads >= 1, "Cannot create a pool with fewer than 1 thread(s).")
  private val executor   = Executors.newFixedThreadPool(threads)
  private val writers    = new CopyOnWriteArrayList[PooledWriter[_]]()
  private val poolClosed = new AtomicBoolean(false)

  /**
    * Wraps a [[Writer[A]]] in the context of an [[AsyncWriterPool]] to provide asynchronous operations.
    * Calls to [[write()]] and related methods place items into the queue, and if necessary, schedule
    * the queue for draining using the [[executor]] in the enclosing [[AsyncWriterPool]].
    */
  class PooledWriter[A] private[AsyncWriterPool] (private val writer: Writer[A], private val queue: BlockingQueue[A]) extends Writer[A] {
    /** A reference to an exception thrown during an asynchronous operation. */
    private val throwable: AtomicReference[Throwable] = new AtomicReference(null)

    /** True if this writer is enqueued to be drained, false otherwise. */
    private val enqueued: AtomicBoolean = new AtomicBoolean(false)

    /** A runnable that can be enqueued to drain this writer's queue to the real output writer. */
    private val drainer = new Runnable {
      /** Drains the queue to the output writer. */
      def drain(): Unit = {
        var item: A = queue.poll()
        while (item != null) {
          writer.write(item)
          item = queue.poll()
        }
      }

      /** Drains the queue, storing any thrown exception into the PooledWriters `throwable`. */
      override def run(): Unit = {
        try     { drain() }
        catch   { case t: Throwable => throwable.compareAndSet(null, t) }
        finally { enqueued.set(false) }
      }
    }

    /** True if a close() method hs been called, even if closing() hasn't happened yet. */
    private val writerCloseRequested: AtomicBoolean = new AtomicBoolean(false)

    /** True if a close() method has been called, false otherwise. */
    private val writerClosed: AtomicBoolean = new AtomicBoolean(false)

    /** Checks to see if an exception has been raised asynchronously, and rethrows it on the current thread. */
    private def checkAndRaise(): Unit = {
      val t = throwable.getAndSet(null)
      if (t != null) throw t
    }

    /** Writes an individual item. May block if a bounded queue is in use. */
    override def write(item: A): Unit = {
      checkAndRaise()
      if (writerCloseRequested.get()) throw new IllegalStateException("Cannot write to closed() writer.")

      // Enqueue the item and optionally enqueue ths writer for output
      this.queue.add(item)
      val previouslyEnqueued = this.enqueued.getAndSet(true)
      if (!previouslyEnqueued) executor.submit(drainer)
    }

    /** Closes the writer _synchronously_ on the current thread. */
    override def close(): Unit = {
      checkAndRaise()
      writerCloseRequested.set(true)
      if (!writerClosed.getAndSet(true)) {
        while (enqueued.get()) Thread.sleep(50)
        if (this.queue.nonEmpty) this.drainer.drain()
        writer.close()
        AsyncWriterPool.this.writers.remove(this)
      }
    }

    /** Closes the writer asynchronously, returning a future that must be accessed to check for exceptions. */
    def closeAsync(): Future[Unit] = {
      checkAndRaise()
      writerCloseRequested.set(true)
      executor.submit(new Callable[Unit] { override def call(): Unit = PooledWriter.this.close() })
    }
  }

  /**
    * Takes a Writer and wraps it into a PooledWriter.  The input Writer should not be accessed directly
    * after this call.  The returned PooledWriter can be written to in a manner identical to the original
    * writer.
    *
    * PooledWriters can used either an unbounded queue for storing to-be-written items (with `queueSize=None`)
    * or a fixed queue of a given size (with `queueSize=Some(n)`). If a fixed size queue is written it is
    * possible for write methods on PooledWriter to block if the queue is full, until some items are removed
    * and written.
    *
    * @param writer the writer to be wrapped and made asynchronous
    * @param queueSize an optional queue size for the writer
    * @tparam A the type of items written to the writer
    * @return a pooled writer that will perform writing asynchronously
    */
  def pool[A](writer: Writer[A], queueSize: Option[Int] = None): PooledWriter[A] = {
    if (poolClosed.get()) throw new IllegalStateException("Pool closed.")

    val queue : BlockingQueue[A] = queueSize match {
      case Some(size) => new ArrayBlockingQueue[A](size)
      case None       => new LinkedBlockingDeque[A]()
    }

    val pooled = new PooledWriter(writer, queue)
    this.writers.add(pooled)
    pooled
  }

  /**
    * Closes the pool and all writers currently within the pool. If any writer throws an exception
    * during close() then an exception will be thrown from this method.  Only the first encountered
    * exception is thrown.
    */
  override def close(): Unit = {
    if (poolClosed.getAndSet(true)) throw new IllegalStateException("Pool already closed.")
    val futures = this.writers.iterator().toIndexedSeq.map { writer => writer.closeAsync() }
    this.executor.shutdown()
    futures.foreach { f => f.get() }
  }
}
