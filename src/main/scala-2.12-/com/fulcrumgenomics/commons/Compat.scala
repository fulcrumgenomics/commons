package com.fulcrumgenomics.commons

import scala.annotation.ClassfileAnnotation

/** Trait that includes types used for cross-building compatibility; scala 2.12 version. */
private[commons] trait Compat {
  /** For annotations available at runtime in 2.12 we use ClassfileAnnotation which has been deprecated in
    * 2.13 and replaced with the newly defined ConstantAnnotation.. */
  type ConstantAnnotation = scala.annotation.ClassfileAnnotation

  /** A mutable grower for building collections. In Scala 2.13 this class was renamed to `GrowableBuilder`. */
  type MutableGrowableBuilder[Elem, To <: scala.collection.generic.Growable[Elem]] = scala.collection.mutable.GrowingBuilder[Elem, To]

  /** For basic views in 2.12 we use the old scala.collection.IterableView which has disappeared in 2.13 to
    * be replaced by View. */
  type View[+A] = scala.collection.IterableView[A, Iterable[A]]
}
