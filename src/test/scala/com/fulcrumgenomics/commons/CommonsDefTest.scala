/*
 * The MIT License
 *
 * Copyright (c) 2015-2016 Fulcrum Genomics LLC
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

package com.fulcrumgenomics.commons

import java.io.{Closeable, IOException}

import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.util.UnitSpec

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.ForkJoinTaskSupport

/**
  * Tests for CommonsDef
  */
class CommonsDefTest extends UnitSpec {
  "CommonsDef.unreachable" should "always throw an exception" in {
    an[RuntimeException] shouldBe thrownBy { unreachable() }
    an[RuntimeException] shouldBe thrownBy { unreachable("booya!") }
    an[RuntimeException] shouldBe thrownBy { None orElse unreachable("booya!") }
    an[RuntimeException] shouldBe thrownBy { None getOrElse unreachable("booya!") }
    Option("foo") getOrElse unreachable("truly unreachable") shouldBe "foo"
  }

  "CommonsDef.yieldAndThen" should "capture the value before running the code block" in {
    val xs = ListBuffer[String]()
    def doIt() = yieldAndThen(xs.length) { xs += "foo" }

    doIt() shouldBe 0
    doIt() shouldBe 1
    doIt() shouldBe 2
    xs shouldBe List("foo", "foo", "foo")
  }

  "CommonsDef.safelyCloseable" should "eat all exceptions" in {
    class C extends Closeable {
      var closed = false
      override def close(): Unit = { closed = true; throw new IOException("screw you!") }
    }

    an[IOException] should be thrownBy new C().close()
    val c = new C
    c.closed shouldBe false
    c.safelyClose()
    c.closed shouldBe true
  }

  private class CleanlyClass {
    var cleanedUp = false
    def doWork(): Boolean = true
    def cleanUp(): Unit = cleanedUp = true
  }

  private class DoWorkExceptionClass {
    var cleanedUp = false
    def doWork(): Boolean = throw new IllegalArgumentException("doWork!")
    def cleanUp(): Unit = cleanedUp = true
  }

  private class CleanUpExceptionClass {
    var cleanedUp = false
    def doWork(): Boolean = true
    def cleanUp(): Unit = throw new IllegalArgumentException("cleanUp!")
  }

  private class DoWorkAndCleanUpExceptionClass {
    var cleanedUp = false
    def doWork(): Boolean = throw new IllegalArgumentException("doWork!")
    def cleanUp(): Unit = throw new IllegalArgumentException("cleanUp!")
  }

  "CommonsDef.tryWith" should "catch all exceptions and close the resource" in {
    // work and cleanup successful
    {
      val resource = new CleanlyClass
      val result   = tryWith(resource)(_.cleanUp()) { r => r.doWork() }
      resource.cleanedUp shouldBe true
      result.isSuccess shouldBe true
      result.get shouldBe true
    }

    // work failure and cleanup successful
    {
      val resource = new DoWorkExceptionClass
      val result   = tryWith(resource)(_.cleanUp()) { r => r.doWork() }
      resource.cleanedUp shouldBe true
      result.isFailure shouldBe true
    }

    // work successful and cleanup failure
    {
      val resource = new CleanUpExceptionClass
      val result   = tryWith(resource)(_.cleanUp()) { r => r.doWork() }
      resource.cleanedUp shouldBe false
      result.isSuccess shouldBe true
      result.get shouldBe true
    }

    // work and cleanup failure
    {
      val resource = new DoWorkAndCleanUpExceptionClass
      val result   = tryWith(resource)(_.cleanUp()) { r => r.doWork() }
      resource.cleanedUp shouldBe false
      result.isFailure shouldBe true
    }
  }

  "forloop(Int,Int,Int)" should "work just like a regular single-index for loop" in {
    val buffer = ListBuffer[Int]()
    forloop (0, 10) { buffer += _ }
    buffer.toList should contain theSameElementsInOrderAs Range(0, 10).toList

    buffer.clear()
    forloop (0, 10, by=2) { buffer += _ }
    buffer.toList should contain theSameElementsInOrderAs Range(0, 10, step=2).toList

    buffer.clear()
    forloop (0, 10, by= -2) { buffer += _ }
    buffer.toList should contain theSameElementsInOrderAs List.empty

    buffer.clear()
    forloop (10, 0, by= -2) { buffer += _ }
    buffer.toList should contain theSameElementsInOrderAs List(10, 8, 6, 4, 2)
  }

  "forloop[T]" should "work like a regular for loop" in {
    val buffer = ListBuffer[String]()
    forloop ("A")(_.length < 6)(_ + "A") { buffer += _ }
    buffer.toList should contain theSameElementsInOrderAs List("A", "AA", "AAA", "AAAA", "AAAAA")
  }

  "ParSupport" should "create a parallel collection with parallelism 2" in {
    val xs = Seq(1, 2, 3, 4, 5).parWith(parallelism = 2)
    xs.tasksupport.asInstanceOf[ForkJoinTaskSupport].forkJoinPool.getParallelism shouldBe 2
    xs.map(_ * 2) shouldBe Seq(2, 4, 6, 8, 10)
  }

  it should "allow setting of parallelism and async mode" in {
    val xs = Seq(1,2).parWith(parallelism=3, fifo=true)
    val pool = xs.tasksupport.asInstanceOf[ForkJoinTaskSupport].forkJoinPool
    pool.getParallelism shouldBe 3
    pool.getAsyncMode shouldBe true

    val ys = Seq(1,2).parWith(parallelism=5, fifo=false)
    val pool2 = ys.tasksupport.asInstanceOf[ForkJoinTaskSupport].forkJoinPool
    pool2.getParallelism shouldBe 5
    pool2.getAsyncMode shouldBe false
  }
}
