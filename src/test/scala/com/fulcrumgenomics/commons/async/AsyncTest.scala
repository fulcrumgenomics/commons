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

import com.fulcrumgenomics.commons.async.Async.{AsyncIterator, AsyncSink, AsyncWriter, AsyncMultiWriter}
import com.fulcrumgenomics.commons.io.Writer
import com.fulcrumgenomics.commons.util.UnitSpec
import org.scalatest.OptionValues

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Random, Try}

class AsyncTest extends UnitSpec with OptionValues {
  "AsyncIterator" should "wrap an empty iterator" in {
    val iter = new AsyncIterator(Iterator[String]())
    iter.hasNext shouldBe false
    iter shouldBe 'empty
    an[NoSuchElementException] should be thrownBy iter.next()
  }

  Seq((10, "fewer"), (20, "the same number of"), (30, "more")).foreach { case (numItems, msg) =>
    it should s"wrap an iterator that has $msg items than bufferSize" in {
      val source = Seq.range(start=0, end=numItems)
      val items = new AsyncIterator(source.toIterator, bufferSize=Some(20)).toList
      items should contain theSameElementsInOrderAs source
    }
  }

  /** Writer useful for testing.  It stores when close() is called, and all items written. */
  private class StringWriter(var block: Boolean = false) extends Writer[String] {
    var closed: Boolean = false
    var items: ListBuffer[String] = new ListBuffer[String]()
    def write(item: String): Unit = {
      while (this.block) {
        Thread.sleep(10)
      }
      items += item
    }
    def close(): Unit = this.closed = true
  }

  private trait RunnableWithResult extends Runnable {
    def done: Boolean
    def result: Option[Try[Unit]]
  }

  /** Returns a [[RunnableWithResult]] that asynchronously executes f, along with a daemon
    * [[Thread]] that is created containing the [[RunnableWithResult]] and is started. */
  private def buildRunnableAndThread[T](f: => Unit): (RunnableWithResult, Thread) = {
    val runnable = new RunnableWithResult {
      def done: Boolean = this.result.isDefined
      var result: Option[Try[Unit]] = None
      def run(): Unit = this.result = try {
        Some(Try { f })
      } catch {
        case ex: InterruptedException => Some(Failure(ex))
      }
    }
    val thread = new Thread(runnable)
    thread.setDaemon(true)
    thread.start()
    (runnable, thread)
  }

  /** Returns a [[RunnableWithResult]] that asynchronously adds the items to the given sink, along with a daemon
    * [[Thread]] that is created containing the [[RunnableWithResult]] and is started. */
  private def buildRunnableAndThreadForSinkAdd[T](sink: AsyncSink[T], items: Traversable[T]): (RunnableWithResult, Thread) = {
    buildRunnableAndThread(items.foreach(sink.add))
  }

  "AsyncSink" should "process no items" in {
    {
      val writer = new StringWriter
      new AsyncSink[String](sink = writer.write, source = Some(writer)).close()
      writer.items shouldBe 'empty
      writer.closed shouldBe true
    }

    // writer's close() method is not passed to AsyncSink
    {
      val writer = new StringWriter
      new AsyncSink[String](sink=writer.write, source=None).close()
      writer.items shouldBe 'empty
      writer.closed shouldBe false
    }
  }

  Seq((10, "fewer"), (20, "the same number of"), (30, "more")).foreach { case (numItems, msg) =>
    it should s"add $msg items than bufferSize" in {
      val bufferSize = 20
      val writer = new StringWriter(block=true)
      val sink = new AsyncSink[String](sink=writer.write, bufferSize=Some(bufferSize), source=Some(writer))
      val items = Seq.range(start=0, end=numItems).map(_.toString)
      val (runnable, thread) = buildRunnableAndThreadForSinkAdd(sink, items)

      if (bufferSize < numItems) { // blocks when adding to the writer, so the queue will fill up, and we'll block adding
        Thread.sleep(100)
        runnable.done shouldBe false
        writer.items shouldBe 'empty
      }
      else { // queue size is more than the # of items, so the runnable should be able to add to the sink
        thread.join(10000) // wait for the thread to complete
        runnable.done shouldBe true
      }
      writer.items shouldBe 'empty

      // unblock
      writer.block = false
      if (bufferSize < numItems) { // wait for the thread to complete adding
        thread.join(10000) // wait for the thread to complete
        runnable.done shouldBe true
      }
      // drain the queue
      writer.closed shouldBe false
      sink.close()
      // all items processed
      writer.items should contain theSameElementsInOrderAs items
      writer.closed shouldBe true
    }
  }

  it should "throw an exception when calling add() after close()" in {
    val writer = new StringWriter
    val sink = new AsyncSink[String](sink = writer.write, source = Some(writer))
    sink.close()
    an[RuntimeException] should be thrownBy sink.add("hello")
  }

  it should "catch and modify an InterruptedException when interrupted when adding to the queue" in {
    val writer = new StringWriter(block=true)
    val sink = new AsyncSink[String](sink = writer.write, source = Some(writer), bufferSize=Some(1))
    val (runnable, thread) = buildRunnableAndThreadForSinkAdd(sink, Seq("A", "B", "C"))
    runnable.done shouldBe false

    // runnable should be blocking in the add() method
    thread.interrupt() // interrupt the add() method
    thread.join()

    // the runnable should be done, but it should have been interrupted
    runnable.done shouldBe true
    runnable.result.value.isSuccess shouldBe false
    runnable.result.value.failed.get.getMessage shouldBe "Interrupted queueing item."
    runnable.result.value.failed.get.isInstanceOf[RuntimeException] shouldBe true
    sink.throwable.get() shouldBe 'empty

    sink.close()
  }

  it should "handle an exception thrown by the provided sink method" in {
    val writer = new Writer[String] {
      def write(item: String): Unit = throw new Exception
      def close(): Unit = Unit
    }
    val sink = new AsyncSink[String](sink = writer.write, source = Some(writer))
    sink.add("item")
    an[Exception] should be thrownBy sink.close()
  }

  it should "catch and modify an InterruptedException when joining on the sink thread in close()" in {
    val writer = new StringWriter(block=true)
    val sink = new AsyncSink[String](sink = writer.write, source = Some(writer))
    val (addRunnable, addThread) = buildRunnableAndThreadForSinkAdd(sink, Seq("A", "B", "C"))
    addRunnable.done shouldBe false

    // addRunnable should be blocking in the add() method
    val (sinkCloseRunnable, sinkCloseThread) = buildRunnableAndThread(sink.close())

    // sinkCloseRunnable should be blocking inside the close() method trying to join on its sinkThread; interrupt it.
    sinkCloseThread.interrupt()
    sinkCloseThread.join()
    sinkCloseRunnable.done shouldBe true
    sinkCloseRunnable.result.value.isSuccess shouldBe false
    sinkCloseRunnable.result.value.failed.get.getMessage shouldBe "Interrupted waiting on sink thread."
    sinkCloseRunnable.result.value.failed.get.isInstanceOf[RuntimeException] shouldBe true
    sink.throwable.get() shouldBe 'empty

    addThread.interrupt()
    addThread.join()
  }

  "AsyncWriter" should "write a bunch of strings" in {
    val random      = new Random(42)
    val writer      = new StringWriter
    val asyncWriter = new AsyncWriter[String](writer)
    val strings     = Seq.range(start=0, end=1000).map { i =>
      val string = random.nextString(80)
      asyncWriter.write(string)
      string
    }
    asyncWriter.close()
    strings should contain theSameElementsInOrderAs writer.items
  }

  "AsyncMultiWriter" should "require at least one writer" in {
    an[Exception] should be thrownBy new AsyncMultiWriter[String](writers=Seq.empty, parallelism=1)
  }

  {
    val random = new Random(42)
    val strings = Seq.range(start=0, end=1000).map { i =>
      math.abs(random.nextInt).toString
    }

    Seq((2, 4), (5, 3), (3, 3)).foreach { case (numWriters, parallelism) =>
      val message = (numWriters, parallelism) match {
        case (n, m) if n < m  => "N < M"
        case (n, m) if n > m  => "N > M"
        case (n, m) if n == m => "N == M"
      }
      it should s"write with N writers and M threads ($message)" in {
        val writers = IndexedSeq.range(start=0, end=numWriters).map { _ => new StringWriter }
        val writer  = new AsyncMultiWriter[String](writers=writers, parallelism=parallelism, bufferSize=Some(5))
        strings.foreach { s => writer.write(s, s.toInt % numWriters) }
        writer.close()
        writers.forall(_.closed) shouldBe true
        strings.groupBy(_.toInt % numWriters).foreach { case (i, strs) =>
          writers(i).items should contain theSameElementsInOrderAs strs
        }
      }
    }
  }
}
