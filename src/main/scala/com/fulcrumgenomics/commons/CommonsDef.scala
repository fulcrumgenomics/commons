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

import java.io.Closeable
import java.util.concurrent.ForkJoinPool

import com.fulcrumgenomics.commons.collection.BetterBufferedIterator
import com.fulcrumgenomics.commons.util.Logger

import scala.collection.Parallelizable
import scala.collection.parallel.{ForkJoinTaskSupport, ParIterableLike, TaskSupport}
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/**
  * Object that is designed to be imported with `import CommonsDef._` in any/all classes
  * much like the way that scala.PreDef is imported in all files automatically.
  *
  * New methods, types and objects should not be added to this class lightly as they
  * will pollute the namespace of any classes which import it.
  */
class CommonsDef {
  /** An exception that implies that code is unreachable. */
  private class UnreachableException(message: String) extends IllegalStateException(message)

  /**
    * A terse way to throw an `UnreachableException` that can be used where any type is expected,
    * e.g. `Option(thing) getOrElse unreachable("my thing is never null")`
    *
    * @param message an optional message
    */
  def unreachable(message: => String = ""): Nothing = throw new UnreachableException(message)


  /**
    * Construct to capture a value, execute some code, and then returned the captured value. Allows
    * code like:
    *   `val x = foo; foo +=1; return x`
    * to be replaced with
    *   `yieldAndThen(foo) {foo +=1}`
    *
    * @param it the value to be returned/yielded
    * @param block a block of code to be evaluated
    * @tparam A the type of thing to be returned (usually inferred)
    * @return it
    */
  def yieldAndThen[A](it: => A)(block: => Unit): A = {
    val retval : A = it
    block
    retval
  }

  /**
   * Implicit class that wraps a closeable and provides a safelyClose method
   * that will not throw any exception.
   */
  implicit class SafelyClosable(private val c: AutoCloseable) {
    def safelyClose() : Unit = {
      try { c.close() }
      catch { case ex: Exception => Unit }
    }
  }

  /** Performs the unit of work on a closeable resource of type [[A]] closing up the resource in the case of an
    * exception or upon completion, and ultimately returning a [[Try]].  An exception during closing does not change the
    * success of the work.  If a logger is provided, the exception is logged, otherwise the exception is ignored.
    *
    * An example would be:
    * {{{
    *   tryWith(Io.toWriter("/path/does/not/exists") { writer => writer.write("Hello World!")) }
    * }}}
    * which throws an exception since the path is not found, but this exception is ignored, and a [[Failure]] is
    * returned.
    *
    * @param resource the resource upon which work is performed.
    * @param doWork the work to perform on the resource.
    * @tparam A the resource type.
    * @return [[Success]] if the work was performed successfully, [[Failure]] otherwise.
    */
  def tryWithCloseable[A <: Closeable](resource: A, logger: Option[Logger] = None)(doWork: A => Unit): Try[Unit] = {
    tryWith(resource, logger)(_.close())(doWork)
  }

  /** Performs the unit of work on a resource of type [[A]] cleaning up the resource in the case of an exception or upon
    * completion, and ultimately returning a [[Try]] of type [[B]].  An exception during cleanup does not change the success
    * of the work.  If a logger is provided, the exception is logged, otherwise the exception is ignored.
    *
    * An example would be:
    * {{{
    *   tryWith(Io.toWriter("/path/does/not/exists")(_.close()) { writer => writer.write("Hello World!")) }
    * }}}
    * which throws an exception since the path is not found, but this exception is ignored, and a [[Failure]] is
    * returned.
    *
    * @param resource the resource upon which work is performed.
    * @param cleanup the clean up method to apply to the resource when the work is complete.
    * @param doWork the work to perform on the resource, returning a result of type [[B]].
    * @tparam A the resource type.
    * @tparam B the result type of the work performed.
    * @return [[Success]] if the work was performed successfully, [[Failure]] otherwise.
    */
  def tryWith[A, B](resource: A, logger: Option[Logger] = None)(cleanup: A => Unit)(doWork: A => B): Try[B] = {
    try {
      Success(doWork(resource))
    } catch {
      case e: Throwable => Failure(e)
    }
    finally {
      try {
        if (resource != null) cleanup(resource)
      } catch {
        case e: Exception => logger.foreach(_.exception(e))
      }
    }
  }

  implicit class MaxNBy[A](val things: TraversableOnce[A]) {
    /** Finds the first `n` elements with the largest values.
      *
      * @param n the number of elements return.
      * @param distinct true to return the first `n` distinct elements, otherwise false to allow duplicate elements.
      * @param cmp the implicit comparison method for type [[A]].
      * @return  the first `n` elements of this traversable or iterator with the largest values.
      */
    def maxN(n: Int, distinct: Boolean = false)(implicit cmp: Ordering[A]): Seq[A] = this.maxNBy[A](n, thing => thing, distinct)

    /** Finds the first `n` elements with the largest values measured by function f.
      *
      * @param n the number of elements return.
      * @param f the measuring function.
      * @param distinct true to return the first `n` distinct elements, otherwise false to allow duplicate elements.
      * @param cmp the implicit comparison method for type [[B]].
      * @tparam B the result type of the function f.
      * @return  the first `n` elements of this traversable or iterator with the largest values measured by function f.
      */
    def maxNBy[B](n: Int, f: A => B, distinct: Boolean = false)(implicit cmp: Ordering[B]): Seq[A] = if (things.isEmpty || n == 0) Seq.empty[A] else {
      // Developer Note: a future improvement would be to determine when to use maxNBySmall vs. maxNByLarge.  Also,
      // if we want to get n items from a sequence that is close to length m, we could find the m-n items to remove, as
      // to save memory.
      things match {
        case _things: Seq[A] => if (_things.length <= n && n < 1024) maxNBySmall(_things, n, f, distinct) else  maxNByLarge(n, f, distinct)
        case _               => maxNByLarge(n, f, distinct)
      }
    }

    /** Companion to [[ThingAndValue]] that provides various apply methods. */
    private object ThingAndValue {
      def apply[B](thing: A, value: B)(implicit cmp: Ordering[B]): ThingAndValue[B] = new UnindexedThingAndValue[B](thing, value)
      def apply[B](thing: A, value: B, index: Int)(implicit cmp: Ordering[B]): ThingAndValue[B] = new IndexedThingAndValue[B](thing, value, index)
    }

    /** The base trait for storing things and values, where the "thing"s can ordered by the "value"s. */
    private sealed trait ThingAndValue[B] extends Comparable[ThingAndValue[B]] {
      implicit def cmp: Ordering[B]
      def thing: A
      def value: B
      override def compareTo(that: ThingAndValue[B]): Int = cmp.compare(that.value, this.value) // sort by the maximum

    }

    /** A little class to store the "things" and their "values". */
    private case class UnindexedThingAndValue[B](thing: A, value: B)(implicit val cmp: Ordering[B]) extends ThingAndValue[B]

    /** A little class to store the "things" and their "values", as well as their index in the original collection.  We
      * use the index to distinguish between duplicates. */
    private case class IndexedThingAndValue[B](thing: A, value: B, index: Int)(implicit val cmp: Ordering[B]) extends ThingAndValue[B] {
      override def compareTo(that: ThingAndValue[B]): Int = that match {
        case _that: IndexedThingAndValue[B] =>
          // sort by maximum value first, then by smaller index
          cmp.compare(_that.value, this.value) match {
            case 0 => this.index.compareTo(_that.index)
            case r => r
          }
        case _ => super.compareTo(that)
      }
    }

    /** Implementation of [[maxNBy()]] for when the # of elements is "small". */
    @inline
    private def maxNBySmall[B](things: Seq[A], n: Int, f: A => B, distinct: Boolean = false)(implicit cmp: Ordering[B]): Seq[A] = {
      if (distinct) things.map(t => ThingAndValue(t, f(t))).sorted.map(_.thing).distinct
      else things.zipWithIndex.map { case (t, i) => ThingAndValue(t, f(t), i) }.sorted.map(_.thing)
    }

    /** Implementation of [[maxNBy()]] for when the # of elements is "large". */
    @inline
    private def maxNByLarge[B](n: Int, f: A => B, distinct: Boolean = false)(implicit cmp: Ordering[B]): Seq[A] = {
      val orderedThings = new java.util.TreeSet[ThingAndValue[B]]()
      val toThingAndValue: (A, Int) => ThingAndValue[B] = {
        if (distinct) (t: A, _: Int) => ThingAndValue(t, f(t)) else (t: A, i: Int) => ThingAndValue(t, f(t), i)
      }
      this.things.toIterator.zipWithIndex.foreach { case (thing, index) =>
        val thingAndValue = toThingAndValue(thing, index)
        // If we haven't reached N yet, add it.  If the last element is "smaller" than the curret one, add it.  Otherwise, don't!
        if (orderedThings.isEmpty || orderedThings.size() < n) orderedThings.add(thingAndValue)
        else {
          // The last thing should exist, so compare it to the current
          val lastThingAndValue = orderedThings.last()
          if (lastThingAndValue.compareTo(thingAndValue) > 0) {
            // Only remove the last thing if the new thing was added
            if (orderedThings.add(thingAndValue)) orderedThings.pollLast()
          }
        }
        require(orderedThings.size <= n, s"$n ${orderedThings.toList}")
      }
      orderedThings.map(_.thing).toList
    }
  }

  /** Implicit class that provides a method to wrap an iterator into a BetterBufferedIterator. */
  implicit class BetterBufferedIteratorScalaWrapper[A](val iterator: Iterator[A]) {
    def bufferBetter = new BetterBufferedIterator(iterator)
  }

  /** Implicit class that provides a method to wrap a Java iterator into a BetterBufferedIterator. */
  implicit class BetterBufferedIteratorJavaWrapper[A](val iterator: java.util.Iterator[A]) {
    def bufferBetter = new BetterBufferedIterator(new JavaIteratorAdapter(iterator))
  }

  /** A trait that combine's scala's Iterator with Java's Iterator. */
  trait DualIterator[A] extends java.util.Iterator[A] with Iterator[A]

  /** Class that wraps a Java iterator into a Scala iterator. */
  private final class JavaIteratorAdapter[A](private[CommonsDef] val underlying: java.util.Iterator[A]) extends DualIterator[A] {
    override def hasNext: Boolean = underlying.hasNext
    override def next(): A = underlying.next()
  }

  /** Class that wraps a Scala iterator into a Java iterator. */
  private final class ScalaIteratorAdapter[A](private[CommonsDef] val underlying: Iterator[A]) extends DualIterator[A] {
    override def hasNext: Boolean = underlying.hasNext
    override def next(): A = underlying.next()
  }

  /** Implicit that wraps a Java iterator as a Scala iterator. */
  implicit def javaIteratorAsScalaIterator[A](iterator: java.util.Iterator[A]): DualIterator[A] = iterator match {
    case iter: DualIterator[A]       => iter
    case iter: java.util.Iterator[A] => new JavaIteratorAdapter[A](iter)
  }

  /** Implicit that wraps a Scala iterator as a Java iterator. */
  implicit def scalaIteratorAsJavaIterator[A](iterator: Iterator[A]): DualIterator[A] = iterator match {
    case iter: DualIterator[A] => iter
    case iter: Iterator[A]     => new ScalaIteratorAdapter[A](iter)
  }

  /** Implicit that converts a Java Iterable into a scala Iterator. */
  implicit def javaIterableToIterator[A](iterable: java.lang.Iterable[A]): Iterator[A] = {
    new JavaIteratorAdapter(iterable.iterator())
  }

  /** Implicit class that provides methods for creating Java collections from an Iterator. */
  implicit class IteratorToJavaCollectionsAdapter[A](private val iterator: Iterator[A]) {
    def toJavaList: java.util.List[A]           = fill(new java.util.ArrayList[A]())
    def toJavaSet : java.util.Set[A]            = fill(new java.util.HashSet[A]())
    def toJavaSortedSet: java.util.SortedSet[A] = fill(new java.util.TreeSet[A]())

    private def fill[C <: java.util.Collection[A]](coll: C): C = {
      iterator.foreach(coll.add)
      coll
    }
  }

  /**
    * An implementation of a for loop that has performance similar to writing a custom
    * while loop for primitive types, which suffer great performance loss when iterating
    * via foreach(), and especially with zipWithIndex.foreach().
    *
    * Equivalent to: for(int i=from; i<until; i+=by) f(i)
    *
    * @param from an initial integer values
    * @param until a value one higher than the last value that should be accepted
    * @param by a step value to increment by each iteration
    * @param f a function that takes the index and is called each iteration
    */
  @inline def forloop(from: Int, until: Int, by: Int=1)(f: Int => Unit): Unit = {
    val comp: (Int,Int) => Boolean = if (by > 0) (_ < _) else (_ > _)
    var i = from
    while (comp(i, until)) {
      f(i)
      i += by
    }
  }

  /**
    * A more generic for loop, that gives performance similar, but slightly worse than,
    * CommonsDef#forloop(Int,Int,Int).  Equivalent to:
    *
    *  for (i=from; check(i); i=next(i)) f(i)
    *
    * @param from an initial value
    * @param check a function that checks to see if a value should be evaluated (not cause termination)
    * @param next a function that takes a value and produced the next value
    * @param f a function called on all values
    * @tparam T the type of the index
    */
  @inline def forloop[@specialized T](from: T)(check: T => Boolean)(next: T => T)(f: T => Unit): Unit = {
    var t = from
    while (check(t)) {
      f(t)
      t = next(t)
    }
  }

  /**
    * Implicit that provides additional methods to sum the values of a sequence after applying a mapping to the items.
    * This has the benefit of not creating any intermediate collections or iterators.   The mapping must map to a
    * [[Numeric]] type.
    * @param sequence the sequence of items to transform and sum.
    * @tparam A the type of items to transform.
    */
  implicit class SumBy[A](sequence: TraversableOnce[A]) {
    /**
      * Applies the given transform to the items in sequence, and then sums them.
      * @param f the transform to map items from type [[A]] to type [[B]].
      * @param x the numeric type of [[B]].
      * @tparam B the type of the items after the transform has been applied; must be a [[Numeric]] type.
      * @return
      */
    def sumBy[B](f: A => B)(implicit x: Numeric[B]): B = sequence.foldLeft(x.zero) { case (left, right) => x.plus(left, f(right)) }
  }

  /**
    * Implicit that provides additional methods to any collection that is Parallelizable.
    * Introduces [[parWith()]] methods that create parallel versions of the collection
    * with various configuration options.
    *
    * @param parallelizable any parallelizable collection
    * @tparam A the type of the elements in the collection
    * @tparam B the type of the parallel representation of the collection
    */
  implicit class ParSupport[A, B <: ParIterableLike[_, _, _]](private val parallelizable: Parallelizable[A,B]) {
    /** Creates a parallel collection with the provided TaskSupport. */
    def parWith(taskSupport: TaskSupport): B = {
      val par = parallelizable.par
      par.tasksupport = taskSupport
      par
    }

    /** Creates a parallel collection with the provided ForkJoinPool. */
    def parWith(pool: ForkJoinPool): B = parWith(taskSupport=new ForkJoinTaskSupport(pool))

    /** Creates a parallel collection with the desired level of parallelism and FIFO semantics. */
    def parWith(parallelism: Int, fifo: Boolean = true): B = {
      parWith(new ForkJoinPool(parallelism, ForkJoinPool.defaultForkJoinWorkerThreadFactory, null, fifo))
    }
  }


  //////////////////////////////////////////////////////////////////////////////////////////
  // Path or String-like type definitions that hint at what the path or string are used for.
  //////////////////////////////////////////////////////////////////////////////////////////

  /** Represents a path to a BAM (or SAM or CRAM) file. */
  type PathToBam = java.nio.file.Path

  /** Represents a path to an intervals file (IntervalList or BED). */
  type PathToIntervals = java.nio.file.Path

  /** Represents a path to a FASTQ file (optionally gzipped). */
  type PathToFastq = java.nio.file.Path

  /** Represents a path to a Reference FASTA file. */
  type PathToFasta = java.nio.file.Path

  /** Represents a path to a VCF/BCF/VCF.gz. */
  type PathToVcf = java.nio.file.Path

  /** Represents a full path including directories, that is intended to be used as a prefix for generating file paths. */
  type PathPrefix = java.nio.file.Path

  /** Represents a path to directory. */
  type DirPath = java.nio.file.Path

  /** Represents a path to a file (not a directory) that doesn't have a more specific type. */
  type FilePath = java.nio.file.Path

  /** A String that represents the prefix or basename of a filename. */
  type FilenamePrefix = String

  /** A String that represents the suffix of a filename, often of the form ".<extension>". */
  type FilenameSuffix = String
}

/** A singleton object providing access to all the functionality of CommonsDef. */
object CommonsDef extends CommonsDef
