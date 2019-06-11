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

import com.fulcrumgenomics.commons.io.Writer

object AsyncWriter {
  /** Builds an [[AsyncWriter]] and starts it.
    *
    * @param writer the writer to wrap
    * @param bufferSize the number of elements to buffer before blocking when writing the elements, or None if unbounded
    * @tparam T the type of object to write
    */
  def apply[T](writer: Writer[T], bufferSize: Option[Int] = None): AsyncWriter[T] = {
    new AsyncWriter[T](writer, bufferSize).start()
  }
}

/** An asynchronous wrapper around a [[Writer]] class.
  *
  * @param writer the writer to wrap
  * @param bufferSize the number of elements to buffer before blocking when writing the elements, or None if unbounded
  * @tparam T the type of object to write
  */
class AsyncWriter[T](val writer: Writer[T], bufferSize: Option[Int] = None)
  extends AsyncSink[T](sink=writer.write, bufferSize=bufferSize, source=Some(writer)) with Writer[T] {
  /** Adds the item to the queue for writing. */
  def write(item: T): Unit = this.add(item)
}
