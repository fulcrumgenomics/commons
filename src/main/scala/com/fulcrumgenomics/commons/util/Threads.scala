/*
 * The MIT License
 *
 * Copyright (c) 2019 Fulcrum Genomics
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

package com.fulcrumgenomics.commons.util

import com.fulcrumgenomics.commons.CommonsDef._

object Threads {

  /** A [[ThreadLocal]] with an initial value produced by a given factory method.
    *
    * @param factory the factory that produces the initial value
    * @tparam A the type of the thread local object
    */
  class IterableThreadLocal[A](factory: () => A) extends ThreadLocal[A] with Iterable[A] {
    private val all = new java.util.concurrent.ConcurrentLinkedQueue[A]()

    /** Returns the initial value for the given thread's thread-local variable. */
    override def initialValue(): A = {
      val a = factory()
      all.add(a)
      a
    }

    /** Returns all the thread-local objects created by the given factory.
      *
      * Care should be taken accessing the iterator since objects may be in use by other threads.
      */
    def iterator: Iterator[A] = all.iterator()
  }
}
