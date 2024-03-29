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
import org.scalatest.OptionValues

class AsyncIteratorTest extends UnitSpec with OptionValues {

  "AsyncIterator" should "wrap an empty iterator" in {
    val iter = new AsyncIterator(Iterator[String]()).start()
    iter.hasNext shouldBe false
    iter.isEmpty shouldBe true
    an[NoSuchElementException] should be thrownBy iter.next()
  }

  Seq((10, "fewer"), (20, "the same number of"), (30, "more")).foreach { case (numItems, msg) =>
    it should s"wrap an iterator that has $msg items than bufferSize" in {
      val source = Seq.range(start=0, end=numItems)
      val items = new AsyncIterator(source.iterator, bufferSize=Some(20)).start().toList
      items should contain theSameElementsInOrderAs source
    }
  }

  it should s"correctly propagate an exception that originates from within the source iterator" in {
    // Issue: https://github.com/fulcrumgenomics/commons/pull/74
    var exceptionNotRaisedInTime = false
    def raise(num: Int): Int = throw new IllegalArgumentException(num.toString)
    val source = Range(1, 10000).iterator.map { num =>
      if (num > 1) exceptionNotRaisedInTime = true // We do this because we can't successfully raise in this context.
      raise(num)
    }
    an[IllegalArgumentException] shouldBe thrownBy { new AsyncIterator(source = source).start().toSeq }
    withClue("Failed because the illegal argument exception was not caught in time:") { exceptionNotRaisedInTime shouldBe false }
  }

  "AsyncIterator.apply" should "start a daemon thread via apply" in {
    val iter = AsyncIterator[String](Iterator("hello world"))
    iter.hasNext() shouldBe true
    iter.next() shouldBe "hello world"
    iter.hasNext() shouldBe false
  }
}
