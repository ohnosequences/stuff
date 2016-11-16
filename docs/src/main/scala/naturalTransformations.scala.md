
```scala
package ohnosequences.stuff

import AnyFunctor._

trait AnyNaturalTransformation {

  type SourceCategory <: AnyCategory
  val sourceCategory: SourceCategory
  type TargetCategory <: AnyCategory
  val targetCategory: TargetCategory

  type SourceFunctor <: SourceCategory ⟶ TargetCategory
  val sourceFunctor: SourceFunctor

  type TargetFunctor <: SourceCategory ⟶ TargetCategory
  val targetFunctor: TargetFunctor

  def at[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]]

  final def apply[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] = at[X]
}

abstract class NaturalTransformation[
  SourceCategory0 <: AnyCategory,
  TargetCategory0 <: AnyCategory,
  SourceFunctor0 <: SourceCategory0 ⟶ TargetCategory0,
  TargetFunctor0 <: SourceCategory0 ⟶ TargetCategory0
]
(
  val sourceCategory: SourceCategory0,
  val sourceFunctor: SourceFunctor0,
  val targetFunctor: TargetFunctor0,
  val targetCategory: TargetCategory0
)
extends AnyNaturalTransformation {

  type SourceCategory = SourceCategory0
  type TargetCategory = TargetCategory0

  type SourceFunctor = SourceFunctor0
  type TargetFunctor = TargetFunctor0
}

case object AnyNaturalTransformation {

  def is[N <: AnyNaturalTransformation](n: N): is[N] =
    n.asInstanceOf[is[N]]

  type is[N <: AnyNaturalTransformation] = N {

    type SourceCategory = N#SourceCategory;
    type TargetCategory = N#TargetCategory;

    type SourceFunctor = N#SourceFunctor;
    type TargetFunctor = N#TargetFunctor;
  }
}

trait AnyIdentityNaturalTransformation extends AnyNaturalTransformation {

  type On <: SourceCategory ⟶ TargetCategory
  val on: On

  lazy val sourceCategory = sourceFunctor.source
  lazy val targetCategory = targetFunctor.target

  type SourceFunctor = On
  lazy val sourceFunctor: SourceFunctor = on

  type TargetFunctor = On
  lazy val targetFunctor: TargetFunctor = on

  final def at[X <: SourceCategory#Objects]: On#Target#C[On#F[X], On#F[X]] = {

    AnyCategory.is(targetCategory).id[On#F[X]]
  }
}

case class IdentityNaturalTransformation[
  SourceCategory0 <: AnyCategory,
  On0 <: SourceCategory0 ⟶ TargetCategory0,
  TargetCategory0 <: AnyCategory
]
(val on: On0) extends AnyIdentityNaturalTransformation {

  type SourceCategory = SourceCategory0
  type TargetCategory = TargetCategory0
  type On = On0
}

```




[test/scala/categories.scala]: ../../test/scala/categories.scala.md
[main/scala/monoidalCategories.scala]: monoidalCategories.scala.md
[main/scala/distributiveLaws.scala]: distributiveLaws.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/monads.scala]: monads.scala.md
[main/scala/syntax/package.scala]: syntax/package.scala.md
[main/scala/syntax/functors.scala]: syntax/functors.scala.md
[main/scala/syntax/categories.scala]: syntax/categories.scala.md
[main/scala/monoidalFunctors.scala]: monoidalFunctors.scala.md
[main/scala/kleisliCoproducts.scala]: kleisliCoproducts.scala.md
[main/scala/functors.scala]: functors.scala.md
[main/scala/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/kleisli.scala]: kleisli.scala.md
[main/scala/categories.scala]: categories.scala.md