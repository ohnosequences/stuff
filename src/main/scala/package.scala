package ohnosequences

package object stuff {
  /*
    ## Functors
  */
  type >->[f0 <: AnyFunctor, g0 <: AnyFunctor { type Source = f0#Target }] =
    FunctorComposition[f0,g0]

  type ⟶[src <: AnyCategory, tgt <: AnyCategory] =
    AnyFunctor {

      type Source = src
      type Target = tgt
    }

  type EndoFunctorOn[Cat <: AnyCategory] =
    AnyFunctor {

      type Source = Cat
      type Target = Cat
    }

  type Monad[C <: AnyCategory, F0 <: EndoFunctorOn[C]] =
    AnyMonad {

      type On = C
      type Functor = F0
    }

  type ~>[src <: AnyFunctor, tgt <: AnyFunctor] =
    AnyNaturalTransformation {

      type SourceCat = SourceF#Source
      type TargetCat = TargetF#Target

      type SourceF = src
      type TargetF = tgt
    }
}
