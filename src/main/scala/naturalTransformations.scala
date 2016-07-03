package ohnosequences.stuff

import AnyFunctor._

trait AnyNaturalTransformation { nat =>

  type SourceCat = SourceF#Source //  <: AnyCategory
  lazy val sourceCat: SourceCat = sourceF.source
  type TargetCat = SourceF#Target // <: AnyCategory
  lazy val targetCat: TargetCat = sourceF.target

  type SourceF <: AnyFunctor // SourceCat ⟶ TargetCat
  val sourceF: SourceF

  type TargetF <: SourceCat ⟶ TargetCat
  // type TargetF <: AnyFunctor { type Source = SourceF#Source; type Target = SourceF#Target }// SourceCat ⟶ TargetCat
  val targetF: TargetF

  def at[X <: SourceF#Source#Objects]: TargetF#Target#C[SourceF#F[X], TargetF#F[X]]

  final def apply[X <: SourceF#Source#Objects]: TargetF#Target#C[SourceF#F[X], TargetF#F[X]] = at[X]

  def >=>[
    M <: AnyNaturalTransformation {
      type SourceF    = nat.TargetF
      type SourceCat  = nat.SourceCat
      type TargetCat  = nat.TargetCat
      type TargetF <: nat.SourceCat ⟶ nat.TargetCat
    }
  ]
  (m: M): AnyVerticalComposition { type First = nat.type; type Second = M } =
    new AnyVerticalComposition {

      type First = nat.type
      val first: First = nat
      type Second = M
      val second = m
    }
}

case object AnyNaturalTransformation {

  def is[N <: AnyNaturalTransformation](n: N): AnyNaturalTransformation.is[N] =
    n.asInstanceOf[AnyNaturalTransformation.is[N]]

  type is[N <: AnyNaturalTransformation] = N with AnyNaturalTransformation {
    //
    // type SourceCat = N#SourceF#Source;
    // type TargetCat = N#TargetF#Target;

    type SourceF = N#SourceF;
    type TargetF = N#TargetF;
  }

  implicit final class Syntax[N <: AnyNaturalTransformation](val n: N) {

    // def >=>[
    //   M <: AnyNaturalTransformation {
    //     type SourceF    = n.TargetF
    //     type SourceCat  = n.SourceCat
    //     type TargetCat  = n.TargetCat
    //     // type TargetF <: SourceCat ⟶ TargetCat
    //   }
    // ](m: M): AnyVerticalComposition { type First = N; type Second = M } = ???
  }
}

trait AnyIdentityNaturalTransformation extends AnyNaturalTransformation {

  type OnF <: AnyFunctor
  val onF: OnF

  type SourceF = AnyFunctor.is[OnF]
  lazy val sourceF: SourceF = AnyFunctor.is(onF)

  // NOTE I don't see how to avoid this here :(
  // type TargetF = onF.type
  type TargetF = AnyFunctor.is[OnF] // OnF { type Source = OnF#Source; type Target = OnF#Target; type F[X <: Source#Objects] = OnF#F[X] }
  lazy val targetF: TargetF = AnyFunctor.is(onF)

  final def at[X <: SourceCat#Objects]: OnF#Target#C[OnF#F[X], OnF#F[X]] = {

    AnyCategory.is(targetCat).id[OnF#F[X]]
  }
}

case class IdentityNaturalTransformation[OnF0 <: AnyFunctor](val onF: OnF0) extends AnyIdentityNaturalTransformation {

  type OnF = OnF0
}

// NOTE this can only be instantiated in ops for natural transformations, due to the dependency on first.
trait AnyVerticalComposition extends AnyNaturalTransformation { composition =>

  type First <: AnyNaturalTransformation
  val first: First //AnyNaturalTransformation.is[First]

  type Second <: AnyNaturalTransformation {
    type SourceF    = first.TargetF
    type SourceCat  = first.SourceCat
    type TargetCat  = first.TargetCat
    type TargetF <: SourceCat ⟶ TargetCat
  }
  val second: Second // AnyNaturalTransformation.is[Second]

  // type SourceCat = First#SourceF#Source
  // override lazy val sourceCat: AnyCategory.is[SourceCat] = AnyCategory.is(first.sourceCat)

  // type TargetCat = Second#TargetF#Target
  // val targetCat: AnyCategory.is[TargetCat] = AnyCategory.is(second.targetCat)

  type SourceF = first.SourceF// AnyFunctor.is[First#SourceF]
  lazy val sourceF: SourceF = AnyFunctor.is[first.SourceF](first.sourceF)
  type TargetF = Second#TargetF // AnyFunctor.is[Second#TargetF]
  lazy val targetF: TargetF = second.targetF

  // NOTE this can improved, I'm sure
  final def at[X <: first.SourceF#Source#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = {

    val zzz: first.TargetF#Target#C[
      first.SourceF#F[X],
      first.TargetF#F[X]
    ] = first.at[X]

    val www: first.TargetF#Target#C[first.TargetF#F[X], Second#TargetF#F[X]] = AnyNaturalTransformation.is(second).at[X]

    AnyCategory.is(first.targetF.target).compose(www, zzz)
  }
}
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
