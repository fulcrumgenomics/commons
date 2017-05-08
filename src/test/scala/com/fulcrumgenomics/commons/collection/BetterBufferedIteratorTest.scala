package com.fulcrumgenomics.commons.collection

import com.fulcrumgenomics.commons.CommonsDef._
import com.fulcrumgenomics.commons.util.UnitSpec

class BetterBufferedIteratorTest extends UnitSpec {
  "BetterBufferedIterator" should "takeWhile and dropWhile without losing elements" in {
    val list = List(1,2,3,4,5,6,7,8,9)
    val xs = list.iterator.bufferBetter
    xs.takeWhile(_ < 5).toSeq shouldBe Seq(1,2,3,4)
    xs.toSeq shouldBe Seq(5,6,7,8,9)

    val ys = list.iterator.bufferBetter
    ys.dropWhile(_ <= 3)
    ys.takeWhile(_ <  7).toSeq shouldBe Seq(4,5,6)
    ys.dropWhile(_ <= 8)
    ys.takeWhile(_ => true).toSeq shouldBe Seq(9)
  }
}
