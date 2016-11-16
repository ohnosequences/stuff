
```scala
package ohnosequences.stuff

final case class FunctorSyntax[functor <: AnyFunctor](val functor: functor) extends AnyVal {

  def >=>[G0 <: AnyFunctor { type Source = functor#Target }](g: G0): functor >=> G0 =
    FunctorComposition(functor,g)

  def id[
    F0  >: functor         <: functor { type Source = C; type Target = D },
    C   >: functor#Source  <: functor#Source,
    D   >: functor#Target  <: functor#Target
  ]
  : IdentityNaturalTransformation[C, F0, D] =
    IdentityNaturalTransformation(functor: F0)
}

```




[test/scala/categories.scala]: ../../../test/scala/categories.scala.md
[main/scala/monoidalCategories.scala]: ../monoidalCategories.scala.md
[main/scala/distributiveLaws.scala]: ../distributiveLaws.scala.md
[main/scala/package.scala]: ../package.scala.md
[main/scala/monads.scala]: ../monads.scala.md
[main/scala/syntax/package.scala]: package.scala.md
[main/scala/syntax/functors.scala]: functors.scala.md
[main/scala/syntax/categories.scala]: categories.scala.md
[main/scala/monoidalFunctors.scala]: ../monoidalFunctors.scala.md
[main/scala/kleisliCoproducts.scala]: ../kleisliCoproducts.scala.md
[main/scala/functors.scala]: ../functors.scala.md
[main/scala/naturalTransformations.scala]: ../naturalTransformations.scala.md
[main/scala/kleisli.scala]: ../kleisli.scala.md
[main/scala/categories.scala]: ../categories.scala.md