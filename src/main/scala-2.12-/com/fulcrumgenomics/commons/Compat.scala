package com.fulcrumgenomics.commons

import scala.annotation.ClassfileAnnotation

/** Trait that includes types used for cross-building compatibility; scala 2.13 version. */
private[commons] trait Compat {
  /** For annotations available at runtime in 2.13 we use ConstantAnnotation. */
  type ConstantAnnotation = scala.annotation.ClassfileAnnotation

  /** For basic views we use the new scala.collection.View. */
  type View[+A] = scala.collection.IterableView[A, Iterable[A]]
}
