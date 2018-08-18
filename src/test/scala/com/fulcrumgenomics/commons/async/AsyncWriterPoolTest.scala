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

import com.fulcrumgenomics.commons.util.UnitSpec

import scala.util.Random

class AsyncWriterPoolTest extends UnitSpec {

  "AsyncWriterPool" should "fail if given < 1 thread" in {
    an[Exception] should be thrownBy new AsyncWriterPool(threads=  0)
    an[Exception] should be thrownBy new AsyncWriterPool(threads= -1)
  }

  {
    val random = new Random(42)
    val strings = Seq.range(start=0, end=100000).map { i => math.abs(random.nextInt).toString }

    for (queueSize <- Seq(None, Some(10), Some(100))) {
      Seq((2, 4), (5, 3), (3, 3)).foreach { case (numWriters, numThreads) =>
        val qs = queueSize match {
          case None    => "unbounded queues"
          case Some(l) => s"$l-length queues"
        }

        it should s"write with $numWriters writers, $numThreads threads and $qs" in {
          val pool    = new AsyncWriterPool(threads=numThreads)
          val writers = IndexedSeq.range(start=0, end=numWriters).map { _ => new StringWriter }
          val pooled  = writers.map(w => pool.pool(w))

          for (string <- strings; writer <- pooled) {
            writer.write(string)
          }

          pool.close()
          writers.forall(_.closed) shouldBe true
          writers.foreach(w => w.items should contain theSameElementsInOrderAs strings)
        }
      }
    }
  }
}
