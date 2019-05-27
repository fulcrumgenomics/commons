package com.fulcrumgenomics.commons.collection

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

import java.io.Closeable
import com.fulcrumgenomics.commons.CommonsDef._

/**
  * An Iterator that implements Closeable and auto-closes when it hits the end of
  * the underlying iterator.
  */
class SelfClosingIterator[A](iter: Iterator[A], private val closer: () => Unit) extends BetterBufferedIterator[A](iter) with Closeable {
  private var open = true

  /** Overridden to ensure close() is called if hasNext is false. */
  override def hasNext: Boolean = if (open && super.hasNext) true else yieldAndThen(false) { close() }

  /**
    * Implements next() such that if there are no more items after the next has been retrieved
    * then the close() method is invoked.
    */
  override def next(): A = {
    if (!open) throw new IllegalStateException("next() called on a closed iterator.")
    val result = super.next()
    if (!hasNext) close()
    result
  }

  /** Invokes the closer function provided in the constructor. */
  override def close(): Unit = if (open) {
    open = false
    this.closer()
  }
}
