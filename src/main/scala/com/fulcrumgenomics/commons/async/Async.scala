/*
 * The MIT License
 *
 * Copyright (c) 2017 Fulcrum Genomics LLC
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
 */

package com.fulcrumgenomics.commons.async

import java.io.Closeable
import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicReference}
import java.util.concurrent.{ArrayBlockingQueue, BlockingQueue, TimeUnit}

import com.fulcrumgenomics.commons.io.Writer

import scala.concurrent.forkjoin.ForkJoinPool

/**
  * Provides generic utility classes related to asynchronous processing, reading, writing, and iterating over items.
  */
object Async {

  /** The default # of items to buffer in queues. */
  private val DefaultBufferSize: Int = 2048


  /** A trait with helper methods that can propagate exceptions from the underlying asynchronous thread.
    *
    * A reference to the underlying thread is given in `thread`. The thread will execute the run() method in a try block.
    * If an exception occurs, throwable will be set if not already defined and uponException will be executed.  Finally,
    * the uponFinally method will be executed.  The thread is set as a daemon but not started.
    *
    * The `checkAndRethrow` method should be used to check if the asynchronous thread has encountered an exception, and if
    * so, rethrow the exception as an [[Error]] or [[RuntimeException]].
    *
    * The `tryAndModifyInterruptedException` method should be used on a block of code that blocks, and will throw a
    * [[RuntimeException]] if the interrupted.
    * */
  private[async] trait AsyncThread {
    protected val isDone: AtomicBoolean = new AtomicBoolean(false)
    private[async] val throwable: AtomicReference[Option[Throwable]] = new AtomicReference[Option[Throwable]](None)
    private val outer = this

    /** The run method to execute in the asynchronous thread. */
    protected def run(): Unit

    /** The method to execute if an exception occurs in the asynchronous thread. */
    protected def uponException(): Unit =  Unit

    /** The method to execute upon successfully execution of the run method or an exception occurs. */
    protected def uponFinally(): Unit = Unit

    /** The asynchronous daemon thread to execute.   */
    protected def buildThread(name: String): Thread = {
      // NB: implicit conversion to Runnable not supported in scala 2.11
      val runnable = new Runnable {
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
      val t = new Thread(runnable, name)
      t.setDaemon(true)
      t
    }

    /** Executes the given block of code.  If an [[InterruptedException]] is thrown, throws a [[RuntimeException]]
      * with the given message.
      *
      * @param message the message to use if an [[InterruptedException]] is thrown by the block of code
      * @param f the block of code to execute
      */
    protected def tryAndModifyInterruptedException[T](message: String)(f: => T): T = {
      try { f } catch {
        case ie: InterruptedException => throw new RuntimeException(message, ie)
      }
    }

    /** Checks to see if an exception has been raised in the asynchronous thread and if so rethrows it as an [[Error]]
      * or [[RuntimeException]] as appropriate.
      */
    protected final def checkAndRethrow(): Unit = {
      this.throwable.getAndSet(None).foreach { thr =>
        this.isDone.set(true) // Ensure no further attempts to run
        thr match {
          case e: Error             => throw e
          case ex: RuntimeException => throw ex
          case t                    => throw new RuntimeException(t)
        }
      }
    }
  }


  /** An asynchronous wrapper for an [[Iterator]].  A separate thread will be created to consume the source iterator.
    * Will buffer up to [[bufferSize]] elements before the blocking when consuming the source.
    *
    * @param source the source iterator to consume
    * @param bufferSize the maximum number of elements to buffer before the blocking when consuming the source
    * @tparam T the type of object to consume
    */
  class AsyncIterator[T](val source: Iterator[T], bufferSize: Int = DefaultBufferSize) extends Iterator[T] with AsyncThread {
    require(bufferSize > 0, "bufferSize must be greater than zero")
    private val queue: BlockingQueue[T] = new ArrayBlockingQueue[T](bufferSize)
    private val thread = buildThread(getClass.getSimpleName)
    private val outer  = this

    // Start the thread after creating the queue
    this.thread.start()

    protected def run(): Unit = outer.source.foreach(outer.queue.put)

    override protected def uponFinally(): Unit = outer.isDone.set(true)

    /** Returns true if there exists more elements, false otherwise */
    def hasNext(): Boolean = {
      checkAndRethrow()

      // If the queue is empty but the underlying thread is not done, then there may or may not be new
      // items that will be put in the qeue.
      while (this.queue.isEmpty && !this.isDone.get()) {
        Thread.sleep(50)
        checkAndRethrow() // check if there was any exception in the underlying thread
      }

      // At this point, either there are items in the queue, or the underlying thread has consumed the source
      if (!this.queue.isEmpty) true
      else { // the queue is empty, and the source has been consumed
        require(this.isDone.get(), "Bug: iterator was not done when the thread was joined.")
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
    * @param bufferSize the number of elements to buffer before blocking when processing the elements
    * @param source the optional source to close when this sink is closed.
    * @tparam T the type of object to process
    */
  class AsyncSink[T](val sink: T => Unit, bufferSize: Int = DefaultBufferSize, val source: Option[{ def close(): Unit }] = None)
    extends Closeable with AsyncThread {
    require(bufferSize > 0, "bufferSize must be greater than zero")
    private val queue: BlockingQueue[T] = new ArrayBlockingQueue[T](bufferSize)
    private val thread = buildThread(this.getClass.getSimpleName + AsyncSink.threadsCreated.incrementAndGet)
    private val outer = this

    // Start the thread after creating the queue
    this.thread.start()

    /** Adds the item to the queue to be processed by [[sink]].
      *
      * @param item the item to queue
      */
    def add(item: T): Unit = {
      if (this.isDone.get) throw new RuntimeException("Attempt to add record to closed sink.")
      checkAndRethrow()
      tryAndModifyInterruptedException("Interrupted queueing item.") { this.queue.put(item) }
      checkAndRethrow()
    }

    /** Attempts to finish draining the queue and then calls `close()` on the source to allow implementation
      * to do any one time clean up.
      */
    def close(): Unit = {
      checkAndRethrow()
      if (!this.isDone.getAndSet(true)) {
        tryAndModifyInterruptedException("Interrupted waiting on sink thread.") { this.thread.join() }
        // The queue should be empty but if it's not, we'll drain it here to protect against any lost data.
        // There's no need to timeout on poll because poll is called only when queue is not empty and
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
      while (!outer.isDone.get || !outer.queue.isEmpty) {
        val item = outer.queue.poll(50, TimeUnit.MILLISECONDS)
        if (item != null) outer.sink(item)
      }
    }

    override protected def uponException(): Unit = {
      // In case the sink was blocking on a full queue before ex has been set, clear the queue
      // so that the sink will no longer be blocked so that it can see the exception.
      outer.queue.clear()
    }
  }

  /** An asynchronous wrapper around a [[Writer]] class.
    *
    * @param writer the writer to wrap
    * @param bufferSize the number of elements to buffer before blocking when writing the elements
    * @tparam T the type of object to write
    */
  class AsyncWriter[T](val writer: Writer[T], bufferSize: Int = DefaultBufferSize)
    extends AsyncSink[T](sink=writer.write, bufferSize=bufferSize, source=Some(writer)) with Writer[T] {
    /** Adds the item to the queue for writing. */
    def write(item: T): Unit = this.add(item)
  }


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
    * @param bufferSize the number of elements per writer to buffer before blocking when writing the elements
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
                            bufferSize: Int = Async.DefaultBufferSize,
                            minimumWaitMillis: Int = 50,
                            maximumWaitMillis: Int = 1000,
                            incrementWaitMillis: Int = 50,
                            decreaseWaitFactor: Double = 0.5
                         ) extends Closeable with AsyncThread with Writer[(T, Int)]{
    require(writers.nonEmpty, "must supply at least one writer")
    require(bufferSize > 0, "bufferSize must be greater than zero")
    require(parallelism > 0, "parallelism must be greater than zero")
    require(decreaseWaitFactor > 0 && decreaseWaitFactor < 1, "decreasePollFactor must be between zero and one exclusive")
    require(minimumWaitMillis <= maximumWaitMillis, "maximumPollMillis < minimumPollMillis")

    private val _writers: Array[Writer[T]] = writers.toArray
    private val queues: Array[BlockingQueue[T]] = _writers.map { _ => new ArrayBlockingQueue[T](bufferSize) }
    private val locks: Array[AtomicBoolean] = _writers.map { _ => new AtomicBoolean(false) }
    private val threads: Seq[Thread] = Range.inclusive(1, parallelism).map { i =>
      buildThread(getClass.getSimpleName + "-thread-" + i)
    }
    private val outer = this

    // Start up the threads
    this.threads.foreach(_.start())

    protected def run(): Unit = {
      var allDone: Boolean = false
      var sleepMillis: Int = minimumWaitMillis
      while (!allDone) {
        // find a non-empty queue
        outer.queues.indexWhere { queue => !queue.isEmpty } match {
          case -1 =>
            allDone = outer.isDone.get() // only exit the loop if all queues are empty and no more items will be added
            Thread.sleep(sleepMillis) // sleep, nothing to do
            sleepMillis = Math.min(maximumWaitMillis, sleepMillis + incrementWaitMillis) // backoff next time
          case i  =>
            val lock  = outer.locks(i)
            if (!lock.getAndSet(true)) { // not previously locked, so this queue is ours now
              try {
                val queue = outer.queues(i)
                // write items until no more in the queue
                val writer = outer._writers(i)
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
      if (this.isDone.get()) throw new RuntimeException("Attempt to add item to a closed writer")
      checkAndRethrow()
      tryAndModifyInterruptedException("Interrupted queueing item.") { this.queues(writerIndex).put(item) }
      checkAndRethrow()
    }

    def close(): Unit = {
      checkAndRethrow()
      if (!this.isDone.getAndSet(true)) {
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
}

