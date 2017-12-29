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

class AsyncWriterTest extends UnitSpec {

  "AsyncWriter" should "write a bunch of strings" in {
    val random      = new Random(42)
    val writer      = new StringWriter
    val asyncWriter = new AsyncWriter[String](writer).start()
    val strings     = Seq.range(start=0, end=1000).map { i =>
      val string = random.nextString(80)
      asyncWriter.write(string)
      string
    }
    asyncWriter.close()
    strings should contain theSameElementsInOrderAs writer.items
  }

  "AsyncWriter.apply" should "start a daemon thread via apply" in {
    val writer = new StringWriter
    val asyncWriter = AsyncWriter[String](writer)
    asyncWriter.write("hello world")
    asyncWriter.close()
    writer.items should contain theSameElementsInOrderAs Seq("hello world")
    writer.closed shouldBe true
  }
}
