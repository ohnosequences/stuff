package ohnosequences.stuff

trait AnyNaturalTransformation {

  type SourceCat <: AnyCategory
  lazy val sourceCat: SourceCat = sourceF.source
  type TargetCat <: AnyCategory
  lazy val targetCat: TargetCat = sourceF.target

  type SourceF <: AnyFunctor { type Source = SourceCat; type Target = TargetCat }
  val sourceF: SourceF

  type TargetF <: AnyFunctor { type Source = SourceCat; type Target = TargetCat }
  val targetF: TargetF

  def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]]

  final def apply[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = at[X]
}

object AnyNaturalTransformation {

  type is[N <: AnyNaturalTransformation] = N with AnyNaturalTransformation {
    type SourceCat = N#SourceCat; type TargetCat = N#TargetCat;
    type SourceF = N#SourceF; type TargetF = N#TargetF;
  }

  type ~>[
    src <: AnyFunctor,
    tgt <: AnyFunctor { type Source = src#Source; type Target = src#Target }
  ] = AnyNaturalTransformation {

    type SourceCat = src#Source
    type TargetCat = src#Target
    type SourceF = src
    type TargetF = tgt
  }
}

trait AnyIdentityNaturalTransformation extends AnyNaturalTransformation {

  type OnF <: AnyFunctor
  val onF: OnF

  type SourceCat = onF.Source
  type TargetCat = onF.Target

  type SourceF = onF.type
  lazy val sourceF: SourceF = onF: onF.type
  type TargetF = onF.type
  lazy val targetF: TargetF = onF: onF.type

  final def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = onF.target.id
}

case class IdentityNaturalTransformation[OnF0 <: AnyFunctor](val onF: OnF0) extends AnyIdentityNaturalTransformation {

  type OnF = OnF0
}


//
// trait AnyVerticalComposition extends AnyNaturalTransformation { composition =>
//
//   import AnyFunctor._, AnyNaturalTransformation._
//
//   type SourceCat <: AnyCategory
//   type TargetCat <: AnyCategory
//
//   type MiddleFunctor <: SourceCat ⟶ TargetCat
//
//   type First <: AnyNaturalTransformation {
//     type SourceCat = composition.SourceCat;
//     type TargetCat = composition.TargetCat;
//     type TargetF = composition.MiddleFunctor;
//   }
//   val first: AnyNaturalTransformation.is[First]
//
//   type Second <: AnyNaturalTransformation {
//     type SourceCat = composition.SourceCat;
//     type TargetCat = composition.TargetCat;
//     type SourceF = composition.MiddleFunctor;
//   }
//   val second: AnyNaturalTransformation.is[Second]
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
