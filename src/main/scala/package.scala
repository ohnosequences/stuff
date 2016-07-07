package ohnosequences

package object stuff {
  /*
    ## Categories
  */
  implicit def morphismsSyntax[
    Cat <: AnyCategory,
    Y <: Cat#Objects,
    Z <: Cat#Objects
  ](g: Cat#C[Y,Z])
  : MorphismsSyntax[Cat,Y,Z] =
    MorphismsSyntax(g)

  implicit def categorySyntax[Cat <: AnyCategory](cat: Cat): CategorySyntax[Cat] =
    CategorySyntax(cat)

  /*
    ## Functors
  */
  type >=>[f0 <: AnyFunctor, g0 <: AnyFunctor { type Source = f0#Target }] =
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

  implicit def functorSyntax[F0 <: AnyFunctor](f0: F0): FunctorSyntax[F0] = FunctorSyntax(f0)

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
