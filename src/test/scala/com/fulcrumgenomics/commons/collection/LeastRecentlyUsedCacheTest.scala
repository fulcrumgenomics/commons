/*
 * The MIT License
 *
 * Copyright (c) 2021 Fulcrum Genomics
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

package com.fulcrumgenomics.commons.collection

import com.fulcrumgenomics.commons.util.UnitSpec
import org.scalatest.OptionValues

class LeastRecentlyUsedCacheTest extends UnitSpec with OptionValues{
  "LeastRecentlyUsedCache" should "put and get a single element" in {
    val cache = new LeastRecentlyUsedCache[Int, Int](1)
    cache.iterator.isEmpty shouldBe true
    cache.put(1, 2) shouldBe None
    cache.get(1).value shouldBe 2
    cache.get(2) shouldBe None
    cache.iterator.toSeq should contain theSameElementsInOrderAs Seq((1, 2))
    cache.size shouldBe 1
  }

  it should "keep only the least recently used when adding more than the max entries" in {
    val cache = new LeastRecentlyUsedCache[Int, Int](3)

    // Up to the limit
    cache.put(1, 2) shouldBe None
    cache.put(2, 3) shouldBe None
    cache.put(3, 4) shouldBe None
    cache.iterator.toSeq should contain theSameElementsInOrderAs Seq((1, 2), (2, 3), (3, 4))

    // Should evict (1, 2)
    cache.put(4, 5) shouldBe None
    cache.iterator.toSeq should contain theSameElementsInOrderAs Seq((2, 3), (3, 4), (4, 5))

    // Should do nothing, as (4, 5) was already the most recently used
    cache.put(4, 5).value shouldBe 5
    cache.iterator.toSeq should contain theSameElementsInOrderAs Seq((2, 3), (3, 4), (4, 5))
    val keyValues = cache.iterator.toSeq

    // Should make (2, 3) the most recently used
    cache.put(2, 3).value shouldBe 3
    cache.iterator.toSeq should contain theSameElementsInOrderAs Seq((3, 4), (4, 5), (2, 3))

    // make sure that keyValues is the ordr from before the cache.put(2, 3)
    keyValues should contain theSameElementsInOrderAs Seq((2, 3), (3, 4), (4, 5))
  }

  it should "remove elements from the cache" in {
    val cache = new LeastRecentlyUsedCache[Int, Int](3)

    // Up to the limit
    cache.put(1, 2) shouldBe None
    cache.put(2, 3) shouldBe None
    cache.put(3, 4) shouldBe None
    cache.iterator.toSeq should contain theSameElementsInOrderAs Seq((1, 2), (2, 3), (3, 4))
    cache.size shouldBe 3

    // Remove non-existent item
    cache.remove(4) shouldBe None

    // Remove the rest of the items
    cache.remove(3).value shouldBe 4
    cache.remove(1).value shouldBe 2
    cache.remove(2).value shouldBe 3

    // Check that there are no elements
    cache.iterator.isEmpty shouldBe true
    cache.size shouldBe 0
    Seq(1, 2, 3).foreach { key => cache.get(key).isEmpty shouldBe true }
  }
}
