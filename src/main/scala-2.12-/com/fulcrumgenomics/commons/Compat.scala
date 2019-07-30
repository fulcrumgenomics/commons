package com.fulcrumgenomics.commons

import scala.annotation.ClassfileAnnotation

/** Trait that includes types used for cross-building compatibility; scala 2.12 version. */
private[commons] trait Compat {
  /** For annotations available at runtime in 2.12 we use ClassfileAnnotation which has been deprecated in
    * 2.13 and replaced with the newly defined ConstantAnnotation.. */
  type ConstantAnnotation = scala.annotation.ClassfileAnnotation

  /** For basic views in 2.12 we use the old scala.collection.IterableView which has disappeared in 2.13 to
    * be replaced by View. */
  type View[+A] = scala.collection.IterableView[A, Iterable[A]]
}
