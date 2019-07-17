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

import java.util.concurrent.CountDownLatch

import com.fulcrumgenomics.commons.util.UnitSpec
import org.scalatest.OptionValues

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.Try

private class TestRunnable(latch: CountDownLatch = new CountDownLatch(0)) extends AsyncRunnable {
  override def execute(): Unit = latch.await()
}

class AsyncRunnableTest extends UnitSpec with OptionValues {

  "AsyncRunnable" should "build a runnable but not start" in {
    val runnable = new TestRunnable()
    runnable.started shouldBe false
    runnable.done shouldBe false
  }

  it should "start but not complete if the run() method never returns" in {
    val runnable = new TestRunnable(new CountDownLatch(1))
    runnable.started shouldBe false
    runnable.start()
    runnable.started shouldBe true
    runnable.done shouldBe false
  }

  it should "start and complete" in {
    val latch = new CountDownLatch(1)
    val runnable = new TestRunnable(latch)
    runnable.started shouldBe false
    runnable.start()
    runnable.started shouldBe true
    latch.countDown()
    runnable.awaitDone()
    latch.await()
    runnable.done shouldBe true
  }

  "AsyncRunnable.start" should "not be able to be called twice" in {
    val runnable = new TestRunnable().start()
    an[Exception] should be thrownBy runnable.start()
  }

  "AsyncRunnable.awaitStarted" should "wait for it to be started" in {
    val runnable = new TestRunnable()
    val thread   = new Thread(runnable)
    thread.setDaemon(true)
    runnable.started shouldBe false
    thread.start()
    runnable.awaitStart()
    runnable.started shouldBe true
  }

  "AsyncRunnable.startAwaitable" should "return an Awaitable that waits for it to be started" in {
    val runnable = new TestRunnable()
    val thread   = new Thread(runnable)
    thread.setDaemon(true)
    runnable.started shouldBe false
    thread.start()
    val awaitable = runnable.uponStart()
    Await.result(awaitable, Duration.Inf)
    runnable.started shouldBe true
  }

  "AsyncRunnable.awaitDone" should "wait for it to be done" in {
    val runnable = new TestRunnable()
    val thread   = new Thread(runnable)
    thread.setDaemon(true)
    runnable.done shouldBe false
    thread.start()
    runnable.awaitDone()
    runnable.done shouldBe true
  }

  "AsyncRunnable.doneAwaitable" should "return an Awaitable that waits for it to be done" in {
    val runnable = new TestRunnable()
    val thread   = new Thread(runnable)
    thread.setDaemon(true)
    runnable.done shouldBe false
    thread.start()
    val awaitable = runnable.uponDone()
    Await.result(awaitable, Duration.Inf)
    runnable.done shouldBe true
  }

  "AsyncRunnable.throwable" should "return the exception thrown in the run() method" in {
    val runnable = new AsyncRunnable {
      override protected def execute(): Unit = require(requirement=false, "exception")
    }.start()
    runnable.started shouldBe true
    runnable.awaitDone()
    runnable.done shouldBe true
    runnable.throwable.isDefined shouldBe true
  }

  "AsyncRunnable.uponException" should "be called when the run() method throws an exception" in {
    val latch = new CountDownLatch(1)
    val runnable = new AsyncRunnable {
      override protected def execute(): Unit = require(requirement=false, "exception")
      override protected def uponException(): Unit = latch.countDown()
    }.start()
    runnable.awaitDone()
    runnable.done shouldBe true
    runnable.throwable.isDefined shouldBe true
    latch.getCount shouldBe 0
  }

  "AsyncRunnable.uponFinally" should "be called when the run() method when the run method completes with an exception" in {
    val latch = new CountDownLatch(1)
    val runnable = new AsyncRunnable {
      override protected def execute(): Unit = require(requirement=false, "exception")
      override protected def uponFinally(): Unit = latch.countDown()
    }.start()
    runnable.awaitDone()
    runnable.done shouldBe true
    runnable.throwable.isDefined shouldBe true
    latch.getCount shouldBe 0
  }

  it should "be called when the run() method when the run method completes with no exception" in {
    val latch = new CountDownLatch(1)
    val runnable = new AsyncRunnable {
      override protected def execute(): Unit = ()
      override protected def uponFinally(): Unit = latch.countDown()
    }.start()
    runnable.awaitDone()
    runnable.done shouldBe true
    runnable.throwable.isDefined shouldBe false
    latch.getCount shouldBe 0
  }

  "AsyncRunnable.checkAndRaise" should "raise a RuntimeException if the run method throws an exception" in {
    val runnable = new AsyncRunnable {
      override protected def execute(): Unit = require(requirement=false, "exception")
      def check(): Unit = checkAndRaise()
    }.start()
    runnable.awaitDone()
    an[RuntimeException] should be thrownBy runnable.check()
  }

  "AsyncRunnable.tryAndModifyInterruptedException" should "re-raise an exception as a RuntimeException when code is interrupted" in {
    // Create an AsyncRunnable that has a method that blocks.  Then create a thread with a runnable that calls the
    // blocking method.  Interrupt the latter thread, and make sure that a RunTimeException was thrown.
    val asyncRunnable = new AsyncRunnable {
      override protected def execute(): Unit = ()
      def waitForIt(): Unit = tryAndModifyInterruptedException("waiting") { Thread.sleep(10000) }
    }.start()
    val waitForItRunnable = new Runnable {
      var result: Option[Try[Unit]] = None
      override def run(): Unit = result = Some(Try { asyncRunnable.waitForIt() } )
    }
    val waitForItThread = new Thread(waitForItRunnable)
    waitForItThread.start()
    waitForItThread.interrupt()
    waitForItThread.join()
    waitForItRunnable.result.value.isSuccess shouldBe false
    waitForItRunnable.result.value.failed.get.getMessage shouldBe "waiting"
    waitForItRunnable.result.value.failed.get.isInstanceOf[RuntimeException] shouldBe true
  }
}
