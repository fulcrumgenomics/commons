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
import java.text.{DecimalFormat, NumberFormat}

import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.CommonsDef.MaxNBy
import com.fulcrumgenomics.commons.collection.BetterBufferedIterator
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

  "CommonsDef.maxN" should "return the maximum N elements" in {
    an[Exception] should be thrownBy Seq.empty[Int].maxN(0)
    Seq.empty[Int].maxN(1) shouldBe 'empty
    val seq    = Seq(5,3,7,1,2,5,8,12,14,22)
    val sorted = seq.sortBy(s => -s)
    Range.inclusive(start=1, end=seq.length*2).foreach { n => // tests asking both fewer and more than the # of elements
      seq.maxN(n) should contain theSameElementsInOrderAs sorted.take(n)
      seq.maxN(n, distinct=true) should contain theSameElementsInOrderAs sorted.distinct.take(n)
    }
  }

  "CommonsDef.maxNBy" should "return the maximum N elements compared by a given method" in {
    an[Exception] should be thrownBy Seq.empty[Int].maxNBy(0, identity)
    Seq.empty[Int].maxNBy(1, identity) shouldBe 'empty
    val seq    = Seq(5,3,7,1,2,5,8,12,14,22)
    val sorted = seq.sorted
    Range.inclusive(start=1, end=seq.length*2).foreach { n => // tests asking both fewer and more than the # of elements
      //seq.maxNBy(n, s => -s) should contain theSameElementsInOrderAs sorted.take(n)
      seq.maxNBy(n, s => -s, distinct=true) should contain theSameElementsInOrderAs sorted.distinct.take(n)
    }
  }


  // Comment this out to get performance testing
  /**
  "CommonsDef.maxN" should "should run quicly" in {
    val r = scala.util.Random
    r.setSeed(13)

    Range.inclusive(start=1, end=100000, step=10000).foreach { len =>
      val values = Range.inclusive(start=1, end=len, step=1).map(_ => r.nextInt)

      Range.inclusive(1, 100, 25).map(_ / 100.0).foreach { frac =>
        val n = (frac * len).toInt
        if (n > 0) {
          val start = System.currentTimeMillis
          values.maxN(n)
          val time = System.currentTimeMillis - start
          println(s"len: $len n: $n time: $time")
        }
      }
    }
  }
  */

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

  "sumBy[B]" should "sum a non-empty sequence of values" in {
    val traversable     = Traversable("1", "2", "3")
    val iterator        = Iterator("1", "2", "3")
    val list            = List("1", "2", "3")

    traversable.sumBy(_.toInt) shouldBe 6
    iterator.sumBy(_.toInt) shouldBe 6
    list.sumBy(_.toInt) shouldBe 6
  }

  "sumBy[B]" should "sum an empty sequence" in {
    val traversable     = Traversable.empty[String]
    val iterator        = Iterator[String]()
    val list            = List.empty[String]

    traversable.sumBy(_.toInt) shouldBe 0
    iterator.sumBy(_.toInt) shouldBe 0
    list.sumBy(_.toInt) shouldBe 0
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

  "CommonsDef implicits" should "create a better buffered iterator" in {
    Seq(1,2,3).iterator.bufferBetter shouldBe an[BetterBufferedIterator[Int]]
  }

  it should "allow interconvert Java and Scala iterators" in {
    def takesJava (iter: java.util.Iterator[String]) = iter.mkString
    def takesScala(iter: scala.collection.Iterator[String]) = iter.mkString

    takesScala(java.util.Arrays.asList("foo", "bar").iterator) shouldBe "foobar"
    takesJava (Seq("bar", "foo").iterator) shouldBe "barfoo"
  }

  it should "convert a java Iterable to a scala iterator" in {
    val xs = java.util.Arrays.asList("foo", "bar")
    val ys = xs.map(x => x + x)
    ys shouldBe an[Iterator[String]]
    ys.mkString shouldBe "foofoobarbar"
  }

  it should "allow toJava* methods on scala iterators" in {
    val xs = Seq(3,2,1,1)
    val list = xs.iterator.toJavaList
    val set  = xs.iterator.toJavaSet
    val sset = xs.iterator.toJavaSortedSet

    list shouldBe an[java.util.List[Int]]
    set  shouldBe an[java.util.Set[Int]]
    sset shouldBe an[java.util.SortedSet[Int]]

    list should contain theSameElementsInOrderAs Seq(3,2,1,1)
    set  should contain theSameElementsAs        Seq(3,2,1)
    sset should contain theSameElementsInOrderAs Seq(1,2,3)
  }
}
