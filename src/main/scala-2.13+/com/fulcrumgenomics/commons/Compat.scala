package com.fulcrumgenomics.commons

/** Trait that includes types used for cross-building compatibility; scala 2.13 version. */
private[commons] trait Compat {
  /** For annotations available at runtime in 2.13 we use ConstantAnnotation. */
  type ConstantAnnotation = scala.annotation.ConstantAnnotation

  /** For basic views we use the new scala.collection.View. */
  type View[+A] = scala.collection.View[A]
}
