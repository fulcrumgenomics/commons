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

import com.fulcrumgenomics.commons.CommonsDef.seqToParSupport

/**
  * Tests for Threads
  */
class ThreadsTest extends UnitSpec {

  "Threads.IterableThreadLocal" should "create thread local objects with a factory for initial values" in {
    val threadLocalValues = Iterator.iterate(1)(i => i + 1)
    val threadLocal       = new Threads.IterableThreadLocal[Int](() => threadLocalValues.synchronized(threadLocalValues.next()))
    val outputValues      = Seq(1, 2, 3, 4, 5)
      .parWith(parallelism = 2)
      .map(_ + threadLocal.get())
      .seq
    outputValues.sum should be > 15
    val expectedValues = Seq.range(start=1, end=threadLocalValues.next())
    threadLocal.iterator.toSeq should contain theSameElementsAs expectedValues
  }
}
