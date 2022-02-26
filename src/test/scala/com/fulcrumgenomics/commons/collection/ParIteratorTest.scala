package com.fulcrumgenomics.commons.collection

import com.fulcrumgenomics.commons.util.UnitSpec

class ParIteratorTest extends UnitSpec {
  "ParIterator" should "map the same as a non-parallel map" in {
    val xs  = Range.inclusive(1, 100)
    val par = ParIterator(xs.iterator, chunkSize=8, threads=4, chunkBuffer=3)
    val out = par.map(_ + 1).map(_ * 2).toAsync().toIndexedSeq
    val exp = xs.map(_ + 1).map(_ * 2)
    out should contain theSameElementsInOrderAs exp
  }

  it should "allow for a more complicated chain of operations" in {
    val xs  = Range.inclusive(1, 100)
    val par = ParIterator(xs.iterator, chunkSize=8, threads=4, chunkBuffer=3)
    val out = par.filter(_ % 3 == 0).flatMap(x => Seq(x, x)).filterNot(_ < 50).toAsync().toIndexedSeq
    val exp =  xs.filter(_ % 3 == 0).flatMap(x => Seq(x, x)).filterNot(_ < 50)
    out should contain theSameElementsInOrderAs  exp
  }

  Seq(0, 2).foreach { chunkBuffer =>
    it should f"propagate errors when chunkBuffer is ${chunkBuffer}" in {
      val xs  = Range.inclusive(1, 100)
      val par = ParIterator(xs.iterator, chunkSize=8, threads=4, chunkBuffer=chunkBuffer)
      an[Exception] shouldBe thrownBy {
        par.map(x => if (x == 50) throw new IllegalArgumentException else x + 1).toAsync().toIndexedSeq
      }
    }
  }

  it should "work fine when filter() removes all elements" in {
    val xs = Range(1, 1000)
    val par = ParIterator(xs.iterator, chunkSize=50, threads=4, chunkBuffer=2)
    val out = par.filter(_ < 0).toIndexedSeq
    out.isEmpty shouldBe true
  }

  it should "collect() the squares of odd numbers from a collection of numbers" in {
    val xs  = Range.inclusive(1, 100)
    val par = ParIterator(xs.iterator, chunkSize=8, threads=4, chunkBuffer=2)
    val out = par.collect { case i: Int if i % 2 == 1=> i*i }.toIndexedSeq
    val exp =  xs.collect { case i: Int if i % 2 == 1=> i*i }
    out should contain theSameElementsInOrderAs  exp
  }

  it should "flatten() a nested collection" in {
    val xs  = Range.inclusive(1, 100).grouped(3).toIndexedSeq
    val par = ParIterator(xs.iterator, chunkSize=8, threads=4, chunkBuffer=2)
    val out = par.flatten.toIndexedSeq
    val exp = xs.flatten
    out should contain theSameElementsInOrderAs  exp
  }
}
