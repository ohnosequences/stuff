
```scala
package ohnosequences.stuff

trait AnyDistributiveLaw {

  type Base <: AnyCategory
  type First <: AnyMonad { type On = Base }
  type Second <: AnyMonad { type On = Base }

  type δ <: (First >=> Second) ~> (Second >=> First)
  val δ: δ
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