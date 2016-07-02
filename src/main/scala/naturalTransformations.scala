package ohnosequences.stuff

trait AnyNaturalTransformation {

  type SourceCat = SourceF#Source // <: AnyCategory
  type TargetCat = SourceF#Target // <: AnyCategory

  type SourceF <: AnyFunctor
  val sourceF: SourceF

  type TargetF <: AnyFunctor { type Source = SourceF#Source; type Target = SourceF#Target }
  val targetF: TargetF

  def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]]

  final def apply[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = at[X]
}

case object AnyNaturalTransformation {

  type is[N <: AnyNaturalTransformation] = N with AnyNaturalTransformation {

    type SourceF = N#SourceF;
    type TargetF = N#TargetF;
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

  type SourceF = OnF
  lazy val sourceF: SourceF = onF

  // NOTE I don't see how to avoid this here :(
  type TargetF = AnyFunctor.is[OnF]
  lazy val targetF: TargetF = AnyFunctor.is(onF)
}

case class IdentityNaturalTransformation[OnF0 <: AnyFunctor](val onF: OnF0) extends AnyIdentityNaturalTransformation {

  type OnF = OnF0

  final def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], OnF#F[X]] = {

    AnyCategory.is(targetF.target).id
  }
}
//
//
// //
// // trait AnyVerticalComposition extends AnyNaturalTransformation { composition =>
// //
// //   // import AnyFunctor._, AnyNaturalTransformation._
// //   //
// //   // type SourceCat <: AnyCategory
// //   // type TargetCat <: AnyCategory
// //
// //   // type MiddleFunctor <: SourceCat ⟶ TargetCat
// //
// //   type First <: AnyNaturalTransformation // {
// //     // type SourceCat  = composition.SourceCat;
// //     // type TargetCat  = composition.TargetCat;
// //     // type TargetF    = composition.MiddleFunctor;
// //   // }
// //   val first: First // AnyNaturalTransformation.is[First]
// //
// //   type Second <: AnyNaturalTransformation {
// //
// //     type SourceF = composition.First#TargetF
// //     // type SourceCat  = composition.SourceCat;
// //     // type TargetCat  = composition.TargetCat;
// //     // type SourceF    = composition.MiddleFunctor;
// //   }
// //   val second: Second // AnyNaturalTransformation.is[Second]
// //
// //   type SourceCat = first.SourceF#Source
// //   type TargetCat = second.TargetF#Target
// //
// //   type SourceF = First#SourceF
// //   lazy val sourceF = first.sourceF
// //   type TargetF = second.TargetF
// //   lazy val targetF = second.targetF
// //
// //   import AnyCategory._
// //
// //   final def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = {
// //
// //     // targetCat.compose( first.at[X], second.at[MiddleFunctor#F[X]]  )
// //     //  >=> second.at
// //     // second.at ∘ first.at
// //     ???
// //   }
// // }
