
```scala
package ohnosequences.stuff

case object syntax {
```


## Categories


```scala
  implicit final def morphismsSyntax[
    Cat <: AnyCategory,
    Y <: Cat#Objects,
    Z <: Cat#Objects
  ](g: Cat#C[Y,Z])(implicit cat: Cat)
  : MorphismsSyntax[Cat,Y,Z] =
    MorphismsSyntax(g)

  implicit final def categorySyntax[Cat <: AnyCategory](cat: Cat): CategorySyntax[Cat] =
    CategorySyntax(cat)

  implicit def functorSyntax[F0 <: AnyFunctor](f0: F0): FunctorSyntax[F0] =
    FunctorSyntax(f0)
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