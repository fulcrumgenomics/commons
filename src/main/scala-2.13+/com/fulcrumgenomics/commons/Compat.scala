package com.fulcrumgenomics.commons

/** Trait that includes types used for cross-building compatibility; scala 2.13 version. */
private[commons] trait Compat {
  /** For annotations available at runtime in 2.13 we use ConstantAnnotation. */
  type ConstantAnnotation = scala.annotation.ConstantAnnotation

  /** For basic views we use the new scala.collection.View. */
  type View[+A] = scala.collection.View[A]

  /** Starting with scala 2.13 the default ordering of Floats is deprecated and we need to pick a preferred ordering. */
  implicit val FloatOrdering: Ordering[Float] = scala.Ordering.Float.TotalOrdering

  /** Starting with scala 2.13 the default ordering of Doubles is deprecated and we need to pick a preferred ordering. */
  implicit val DoubleOrdering: Ordering[Double] = scala.Ordering.Double.TotalOrdering
}
