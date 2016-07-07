
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

// NOTE this can only be instantiated in ops for natural transformations, due to the dependency on first.
// trait AnyVerticalComposition extends AnyNaturalTransformation { composition =>
//
//   type First <: AnyNaturalTransformation
//   val first: First //AnyNaturalTransformation.is[First]
//
//   type Second <: AnyNaturalTransformation {
//     type SourceF    = first.TargetF
//     type SourceCat  = first.SourceCat
//     type TargetCat  = first.TargetCat
//     type TargetF <: SourceCat ⟶ TargetCat
//   }
//   val second: Second // AnyNaturalTransformation.is[Second]
//
//   type SourceCat = first.SourceCat
//   lazy val sourceCat: SourceCat = first.sourceCat // AnyCategory.is(first.sourceCat)
//   type TargetCat = second.TargetCat
//   lazy val targetCat = second.targetCat
//
//   // type TargetCat = Second#TargetF#Target
//   // val targetCat: AnyCategory.is[TargetCat] = AnyCategory.is(second.targetCat)
//
//   type SourceF = first.SourceF// AnyFunctor.is[First#SourceF]
//   lazy val sourceF: SourceF = AnyFunctor.is[first.SourceF](first.sourceF)
//   type TargetF = second.TargetF // AnyFunctor.is[Second#TargetF]
//   lazy val targetF: TargetF = second.targetF
//
//   // NOTE this can improved, I'm sure
//   final def at[X <: first.SourceF#Source#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = {
//
//     val zzz: first.TargetF#Target#C[
//       first.SourceF#F[X],
//       first.TargetF#F[X]
//     ] = first.at[X]
//
//     val www: first.TargetF#Target#C[first.TargetF#F[X], second.TargetF#F[X]] = second.at[X]
//
//     AnyCategory.is(first.targetF.target).compose(www, zzz)
//   }
// }
//
// case class VerticalComposition[
//   F0 <: AnyNaturalTransformation,
//   S0 <: AnyNaturalTransformation {
//     type SourceCat = F0#SourceCat
//     type TargetCat = F0#TargetCat
//     type SourceF = F0#TargetF
//   }
// ](val _first: F0, val _second: S0) extends AnyVerticalComposition {
//
//   type SourceCat = F0#SourceCat
//   lazy val sourceCat: SourceCat = first.sourceCat
//   lazy val targetCat: TargetCat = first.targetCat
//
//   type TargetCat = F0#TargetCat
//
//   type First = AnyNaturalTransformation.is[F0]
//   val first: First = AnyNaturalTransformation.is(_first)
//
//   type Second = AnyNaturalTransformation.is[S0]
//   val second: Second = AnyNaturalTransformation.is(_second)
// }
//
// trait AnyHorizontalComposition extends AnyNaturalTransformation {
//
//   type C1 <: AnyCategory;
//   type C2 <: AnyCategory;
//   type C3 <: AnyCategory
//   val c3: AnyCategory.is[C3]
//
//   type F1 <: C1 ⟶ C2
//   type F2 <: C2 ⟶ C3
//   val F2: AnyFunctor.is[F2]
//
//   type G1 <: C1 ⟶ C2
//   type G2 <: C2 ⟶ C3
//
//   type First <: AnyNaturalTransformation {
//
//     type SourceCat = C1;
//     type TargetCat = C2;
//
//     type SourceF = F1;
//     type TargetF = G1;
//   }
//   val first: AnyNaturalTransformation.is[First]
//
//   type Second <: AnyNaturalTransformation {
//
//     type SourceCat = C2;
//     type TargetCat = C3;
//
//     type SourceF = F2;
//     type TargetF = G2;
//   }
//   val second: AnyNaturalTransformation.is[Second]
//
//   type SourceCat = C1
//   type TargetCat = C3
//
//   type SourceF = F1 >=> F2
//   type TargetF = G1 >=> G2
//
//   def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = {
//
//     c3.compose( second.at, F2(first.at[X]) )
//   }
// }

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