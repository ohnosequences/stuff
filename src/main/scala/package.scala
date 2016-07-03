package ohnosequences

package object stuff {

  type >=>[f0 <: AnyFunctor, g0 <: AnyFunctor { type Source = f0#Target }] =
    FunctorComposition[f0,g0]

  type ⟶[src <: AnyCategory, tgt <: AnyCategory] = AnyFunctor {

     type Source = src;
     type Target = tgt;
   }

  type ~>[src <: AnyFunctor, tgt <: AnyFunctor] = AnyNaturalTransformation {

    type SourceF <: src;
    type TargetF <: tgt;
  }
}
