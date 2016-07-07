
```scala
package ohnosequences

package object stuff {
```


## Categories


```scala
  implicit def morphismsSyntax[
    Cat <: AnyCategory,
    Y <: Cat#Objects,
    Z <: Cat#Objects
  ](g: Cat#C[Y,Z])
  : MorphismsSyntax[Cat,Y,Z] =
    MorphismsSyntax(g)

  implicit def categorySyntax[Cat <: AnyCategory](cat: Cat): CategorySyntax[Cat] =
    CategorySyntax(cat)
```


## Functors


```scala
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

```




[test/scala/categories.scala]: ../../test/scala/categories.scala.md
[main/scala/monoidalCategories.scala]: monoidalCategories.scala.md
[main/scala/distributiveLaws.scala]: distributiveLaws.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/monads.scala]: monads.scala.md
[main/scala/monoidalFunctors.scala]: monoidalFunctors.scala.md
[main/scala/functors.scala]: functors.scala.md
[main/scala/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/kleisli.scala]: kleisli.scala.md
[main/scala/categories.scala]: categories.scala.md