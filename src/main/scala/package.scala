package ohnosequences

package object stuff {

  type ~>[src <: AnyFunctor, tgt <: AnyFunctor] = AnyNaturalTransformation {

    type SourceF = src;
    type TargetF = tgt;
  }
}
