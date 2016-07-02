package ohnosequences.stuff

import AnyFunctor._

trait AnyNaturalTransformation {

  type SourceCat <: AnyCategory
  type TargetCat <: AnyCategory

  type SourceF <: SourceCat ⟶ TargetCat
  val sourceF: SourceF

  type TargetF <: SourceCat ⟶ TargetCat
  val targetF: TargetF

  def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]]

  final def apply[X <: SourceF#Source#Objects]: TargetF#Target#C[SourceF#F[X], TargetF#F[X]] = at[X]
}

case object AnyNaturalTransformation {

  def is[N <: AnyNaturalTransformation](n: N): AnyNaturalTransformation.is[N] = n.asInstanceOf[AnyNaturalTransformation.is[N]]

  type is[N <: AnyNaturalTransformation] = N with AnyNaturalTransformation {
    //
    type SourceCat = N#SourceCat;
    type TargetCat = N#TargetCat;

    type SourceF = N#SourceF;
    type TargetF = N#TargetF;
  }

  type ~>[
    src <: AnyFunctor,
    tgt <: AnyFunctor // { type Source = src#Source; type Target = src#Target }
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

  type SourceCat = OnF#Source
  type TargetCat = OnF#Target

  // NOTE I don't see how to avoid this here :(
  type SourceF = AnyFunctor.is[OnF]
  lazy val sourceF: SourceF = AnyFunctor.is(onF)

  type TargetF = AnyFunctor.is[OnF]
  lazy val targetF: TargetF = AnyFunctor.is(onF)
}

case class IdentityNaturalTransformation[OnF0 <: AnyFunctor](val onF: OnF0) extends AnyIdentityNaturalTransformation {

  type OnF = OnF0

  final def at[X <: SourceCat#Objects]: TargetCat#C[OnF#F[X], OnF#F[X]] = {

    AnyCategory.is(targetF.target).id
  }
}

trait AnyVerticalComposition extends AnyNaturalTransformation {

  // type First <: AnyNaturalTransformation
  // val first: First
  //
  // type Second <: AnyNaturalTransformation {
  //
  //   type SourceF = First#TargetF // { type Source = First#SourceF#Source }
  //
  //   type TargetF <: AnyFunctor {
  //     type Source = First#SourceF#Source
  //     type Target = First#SourceF#Target
  //   }
  // }
  // val second: Second
  //
  // type SourceF = First#SourceF
  // lazy val sourceF: SourceF = first.sourceF
  // type TargetF >: Second#TargetF <: Second#TargetF
  // lazy val targetF: TargetF = second.targetF
}

// case class VerticalComposition[
//   F0 <: AnyNaturalTransformation,
//   S0 <: AnyNaturalTransformation {
//     type SourceF = F0#TargetF { type Source = F0#SourceF#Source }
//     type TargetF <: AnyFunctor {
//       type Source = F0#SourceF#Source
//       type Target = F0#SourceF#Target
//     }
//   }
// ](val first: F0, val second: S0) extends AnyVerticalComposition {
//
//   type First  = F0
//   type Second = S0
//
//   final def at[X <: SourceF#Source#Objects]: TargetF#Target#C[SourceF#F[X], TargetF#F[X]] = {
//
//     // val tf: AnyFunctor.is[F0]
//     val cat: AnyCategory.is[F0#SourceF#Target] = AnyCategory.is(targetF.target)
//
//     val f = AnyNaturalTransformation.is[First](first)
//     val s: AnyNaturalTransformation.is[S0] = AnyNaturalTransformation.is[S0](second)
//
//     val tf: AnyFunctor.is[TargetF] = AnyFunctor.is(s.targetF)
//     val tc: AnyCategory.is[Second#TargetF#Target] = AnyCategory.is(tf.target)
//     // import cat._
//
//     // val zz: TargetF#Target#C[SourceF#F[X], First#TargetF#F[X]] = s.at[X]
//
//     tc.compose[F0#SourceF#F[X],F0#TargetF#F[X],S0#TargetF#F[X]]( s.at[X], f.at[X] )
//     // cat.compose[F0#SourceF#F[X],F0#TargetF#F[X],S0#TargetF#F[X]]( f.at[X], second.at[X] )
//     // val tF: AnyFunctor.is[F0#TargetF] = AnyFunctor.is[Second#TargetF](second.targetF)
//     // val tCat: AnyCategory.is[F0#TargetF#Target] = AnyCategory.is[Second#TargetF#Target](tF.target)
//
//     // (tCat: TargetF#Target).compose(
//     //   AnyNaturalTransformation.is[First](first).at[X],
//     //   AnyNaturalTransformation.is[Second](second).at[First#TargetF#F[X]] )
//     //  >=> second.at
//     // second.at ∘ first.at
//     ???
//   }
// }

//
// trait AnyVerticalComposition extends AnyNaturalTransformation { composition =>
//
//   // import AnyFunctor._, AnyNaturalTransformation._
//   //
//   // type SourceCat <: AnyCategory
//   // type TargetCat <: AnyCategory
//
//   // type MiddleFunctor <: SourceCat ⟶ TargetCat
//
//   type First <: AnyNaturalTransformation // {
//     // type SourceCat  = composition.SourceCat;
//     // type TargetCat  = composition.TargetCat;
//     // type TargetF    = composition.MiddleFunctor;
//   // }
//   val first: First // AnyNaturalTransformation.is[First]
//
//   type Second <: AnyNaturalTransformation {
//
//     type SourceF = composition.First#TargetF
//     // type SourceCat  = composition.SourceCat;
//     // type TargetCat  = composition.TargetCat;
//     // type SourceF    = composition.MiddleFunctor;
//   }
//   val second: Second // AnyNaturalTransformation.is[Second]
//
//   type SourceCat = first.SourceF#Source
//   type TargetCat = second.TargetF#Target
//
//   type SourceF = First#SourceF
//   lazy val sourceF = first.sourceF
//   type TargetF = second.TargetF
//   lazy val targetF = second.targetF
//
//   import AnyCategory._
//
//   final def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = {
//
//     // targetCat.compose( first.at[X], second.at[MiddleFunctor#F[X]]  )
//     //  >=> second.at
//     // second.at ∘ first.at
//     ???
//   }
// }
