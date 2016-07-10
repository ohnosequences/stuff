
```scala
package ohnosequences.stuff

import AnyFunctor._

trait AnyNaturalTransformation {

  type SourceCat <: AnyCategory
  val sourceCat: SourceCat
  type TargetCat <: AnyCategory
  val targetCat: TargetCat

  type SourceF <: SourceCat ⟶ TargetCat
  val sourceF: SourceF

  type TargetF <: SourceCat ⟶ TargetCat
  val targetF: TargetF

  def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]]

  final def apply[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = at[X]
}

abstract class NaturalTransformation[
  SourceCat0 <: AnyCategory,
  TargetCat0 <: AnyCategory,
  SourceF0 <: SourceCat0 ⟶ TargetCat0,
  TargetF0 <: SourceCat0 ⟶ TargetCat0
]
(
  val sourceCat: SourceCat0,
  val sourceF: SourceF0,
  val targetF: TargetF0,
  val targetCat: TargetCat0
)
extends AnyNaturalTransformation {

  type SourceCat = SourceCat0
  type TargetCat = TargetCat0

  type SourceF = SourceF0
  type TargetF = TargetF0
}

case object AnyNaturalTransformation {

  def is[N <: AnyNaturalTransformation](n: N): is[N] =
    n.asInstanceOf[is[N]]

  type is[N <: AnyNaturalTransformation] = N {
    //
    type SourceCat = N#SourceCat;
    type TargetCat = N#TargetCat;

    type SourceF = N#SourceF;
    type TargetF = N#TargetF;
  }
}

trait AnyIdentityNaturalTransformation extends AnyNaturalTransformation {

  type OnF <: SourceCat ⟶ TargetCat
  val onF: OnF

  lazy val sourceCat = sourceF.source
  lazy val targetCat = targetF.target

  type SourceF = OnF
  lazy val sourceF: SourceF = onF

  type TargetF = OnF
  lazy val targetF: TargetF = onF

  final def at[X <: SourceCat#Objects]: OnF#Target#C[OnF#F[X], OnF#F[X]] = {

    AnyCategory.is(targetCat).id[OnF#F[X]]
  }
}

case class IdentityNaturalTransformation[
  SourceCat0 <: AnyCategory,
  OnF0 <: SourceCat0 ⟶ TargetCat0,
  TargetCat0 <: AnyCategory
]
(val onF: OnF0) extends AnyIdentityNaturalTransformation {

  type SourceCat = SourceCat0
  type TargetCat = TargetCat0
  type OnF = OnF0
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