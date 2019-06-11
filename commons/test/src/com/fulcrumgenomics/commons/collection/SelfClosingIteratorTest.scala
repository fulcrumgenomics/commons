package com.fulcrumgenomics.commons.collection

import com.fulcrumgenomics.commons.util.UnitSpec

class SelfClosingIteratorTest extends UnitSpec {
  "SelfClosingIterator" should "return all the items and then call the close function" in {
    var closed = false
    val iter = new SelfClosingIterator(Seq(1,2,3,4,5).iterator, () => closed = true)
    iter.take(3).toSeq shouldBe Seq(1,2,3)

    iter.next shouldBe 4
    closed shouldBe false

    iter.head shouldBe 5
    closed shouldBe false

    iter.next shouldBe 5
    closed shouldBe true
    iter.hasNext shouldBe false
    an[Exception] shouldBe thrownBy { iter.next() }
  }

  it should "call close even on an empty iterator" in {
    var closed = false
    val iter = new SelfClosingIterator(Iterator.empty, () => closed = true)
    closed shouldBe false
    iter.foreach { x => fail("Should not do anything on an empty iterator!") }
    closed shouldBe true
  }

  it should "handle iterators that don't like being closed twice" in {
    var closings = 0
    val iter = new SelfClosingIterator(Seq(1,2,3).iterator, () => { closings += 1; if (closings > 1) fail("Too many!")})
    val total = iter.sum
    iter.close()
    iter.close()
    closings shouldBe 1
  }

  it should "treat closed iterators as exhausted iterators even if there could be more" in {
    var closed = false
    val iter = new SelfClosingIterator(Seq(1,2,3).iterator, () => closed = true)
    iter.next() shouldBe 1
    iter.close()
    iter.hasNext shouldBe false
    an[Exception] shouldBe thrownBy { iter.next() }
  }
}
