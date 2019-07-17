/*
 * The MIT License
 *
 * Copyright (c) 2016 Fulcrum Genomics LLC
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

package com.fulcrumgenomics.commons.collection

import com.fulcrumgenomics.commons.CommonsDef._

/** A little trait to allow BetterBufferedIterator to override the headOption method. The latter method
  * is not found in [[BetterBufferedIterator]] in 2.11.11, but in 2.12.2! */
sealed trait HeadOption[A] {
  def headOption: Option[A]
}

/**
  * A better buffered iterator that provides implementations of takeWhile and dropWhile
  * that don't discard extra values.
  */
class BetterBufferedIterator[A](private val iter: Iterator[A]) extends HeadOption[A] with BufferedIterator[A] {
  private var buffer: Option[A] = maybeNext

  /** Returns a Some(A) if there is a next in the iterator else None. */
  private def maybeNext = if (this.iter.hasNext) Some(this.iter.next()) else None

  /** Returns the next item in the iterator without consuming it. */
  override def head: A = this.buffer.get

  /** True if head() and hasNext() will return an item, false otherwise. */
  override def hasNext: Boolean = this.buffer.nonEmpty

  /** Consumes the next item from the iterator. */
  override def next(): A = yieldAndThen(buffer.get) { this.buffer = maybeNext }

  /**
    * Returns an iterator over contiguous elements that match the predicate.
    */
  override def takeWhile(p: (A) => Boolean): Iterator[A] = {
    val outer = this
    new Iterator[A] {
      def hasNext: Boolean = outer.hasNext && p(outer.head)
      def next(): A = outer.next()
    }
  }

  /** Drops items while they match the predicate. */
  override def dropWhile(p: (A) => Boolean): this.type = {
    while (hasNext && p(head)) next()
    this
  }

  /** Returns an option of the next element of an iterator without advancing beyond it.
    * @return  the next element of this iterator if it has a next element
    *           `None` if it does not
    */
  override def headOption: Option[A] = if (hasNext) Some(head) else None
}
