package ohnosequences.stuff

trait AnyNatT { nat =>

  type SourceCat <: AnyCategory
  lazy val sourceCat: AnyCategory.is[SourceCat] = sourceF.source
  type TargetCat <: AnyCategory
  lazy val targetCat: AnyCategory.is[TargetCat] = targetF.target

  type SourceF <: AnyFunctor { type Source = SourceCat; type Target = TargetCat }
  val sourceF: AnyFunctor.is[SourceF]

  type TargetF <: AnyFunctor { type Source = SourceCat; type Target = TargetCat }
  val targetF: AnyFunctor.is[TargetF]

  def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]]

  final def apply[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = at[X]
}

object AnyNatT {

  type is[N <: AnyNatT] = N with AnyNatT {
    type SourceCat = N#SourceCat; type TargetCat = N#TargetCat;
    type SourceF = N#SourceF; type TargetF = N#TargetF;
  }

  // type between[src <: AnyFunctor, tgt <: AnyFunctor.between[src#Source, src#Target]] = AnyNatT {
  //
  //   type SourceCat = src#Source
  //   type TargetCat = src#Target
  //   type SourceF = src
  //   type TargetF = tgt
  // }

  type ~>[
    src <: AnyFunctor,
    tgt <: AnyFunctor { type Source = src#Source; type Target = src#Target }
  ] = AnyNatT {

    type SourceCat = src#Source
    type TargetCat = src#Target
    type SourceF = src
    type TargetF = tgt
  }
}


trait AnyIdNatT extends AnyNatT {

  type OnF <: AnyFunctor // { type Source = SourceCat; type Target = TargetCat }
  val onF: AnyFunctor.is[OnF]

  type SourceCat = OnF#Source
  type TargetCat = OnF#Target

  type SourceF = AnyFunctor.is[OnF]
  lazy val sourceF = onF
  type TargetF = AnyFunctor.is[OnF]
  lazy val targetF = onF

  final def at[X <: SourceCat#Objects]: TargetCat#C[OnF#F[X], OnF#F[X]] = targetCat.id[OnF#F[X]]
}

class IdNatT[OnF0 <: AnyFunctor](val onF: AnyFunctor.is[OnF0]) extends AnyIdNatT {

  type OnF = OnF0
}
//
// trait AnyVerticalComposition extends AnyNatT { composition =>
//
//   import AnyFunctor._, AnyNatT._
//
//   type SourceCat <: AnyCategory
//   type TargetCat <: AnyCategory
//
//   type MiddleFunctor <: SourceCat ⟶ TargetCat
//
//   type First <: AnyNatT {
//     type SourceCat = composition.SourceCat;
//     type TargetCat = composition.TargetCat;
//     type TargetF = composition.MiddleFunctor;
//   }
//   val first: AnyNatT.is[First]
//
//   type Second <: AnyNatT {
//     type SourceCat = composition.SourceCat;
//     type TargetCat = composition.TargetCat;
//     type SourceF = composition.MiddleFunctor;
//   }
//   val second: AnyNatT.is[Second]
//
//   type SourceF = First#SourceF
//   lazy val sourceF = first.sourceF
//   type TargetF = Second#TargetF
//   lazy val targetF = second.targetF
//
//   import AnyCategory._
//   lazy val inTargetCat = CategoryModule(targetCat)
//
//   final def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = {
//
//     import inTargetCat._
//
//     second.at ∘ first.at
//   }
// }
