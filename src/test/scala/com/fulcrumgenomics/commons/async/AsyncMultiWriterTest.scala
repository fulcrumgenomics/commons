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

class AsyncMultiWriterTest extends UnitSpec {

  "AsyncMultiWriter" should "require at least one writer" in {
    an[Exception] should be thrownBy new AsyncMultiWriter[String](writers=Seq.empty, parallelism=1)
  }

  {
    val random = new Random(42)
    val strings = Seq.range(start=0, end=1000).map { i =>
      math.abs(random.nextInt).toString
    }

    Seq((2, 4), (5, 3), (3, 3)).foreach { case (numWriters, parallelism) =>
      val message = (numWriters, parallelism) match {
        case (n, m) if n < m  => "N < M"
        case (n, m) if n > m  => "N > M"
        case (n, m) if n == m => "N == M"
      }
      it should s"write with N writers and M threads ($message)" in {
        val writers = IndexedSeq.range(start=0, end=numWriters).map { _ => new StringWriter }
        val writer  = new AsyncMultiWriter[String](writers=writers, parallelism=parallelism, bufferSize=Some(5))
        strings.foreach { s => writer.write(s, s.toInt % numWriters) }
        writer.close()
        writers.forall(_.closed) shouldBe true
        strings.groupBy(_.toInt % numWriters).foreach { case (i, strs) =>
          writers(i).items should contain theSameElementsInOrderAs strs
        }
      }
    }
  }
}
