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
  val sourceCat: SourceCat = onF.source
  type TargetCat = OnF#Target
  val targetCat: TargetCat = onF.target

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

trait AnyVerticalComposition extends AnyNaturalTransformation { composition =>

  type First <: AnyNaturalTransformation {
    type SourceCat = composition.SourceCat
    type TargetCat = composition.TargetCat
  }
  val first: First

  type Second <: AnyNaturalTransformation {

    type SourceCat = First#SourceCat
    type TargetCat = First#TargetCat
    type SourceF = First#TargetF
  }
  val second: Second

  type SourceF = AnyFunctor.is[First#SourceF]
  lazy val sourceF: SourceF = AnyFunctor.is[First#SourceF](first.sourceF)
  type TargetF = Second#TargetF
  lazy val targetF: TargetF = second.targetF

  final def at[X <: SourceCat#Objects]: TargetCat#C[First#SourceF#F[X], TargetF#F[X]] = {

    AnyCategory.is(targetCat).compose( AnyNaturalTransformation.is[Second](second).at[X], AnyNaturalTransformation.is[First](first).at[X] )
  }
}

case class VerticalComposition[
  F0 <: AnyNaturalTransformation,
  S0 <: AnyNaturalTransformation {
    type SourceCat = F0#SourceCat
    type TargetCat = F0#TargetCat
    type SourceF = F0#TargetF
  }
](val _first: F0, val _second: S0) extends AnyVerticalComposition {

  type SourceCat = F0#SourceCat
  lazy val sourceCat: SourceCat = first.sourceCat
  lazy val targetCat: TargetCat = first.targetCat

  type TargetCat = F0#TargetCat

  type First = AnyNaturalTransformation.is[F0]
  val first: First = AnyNaturalTransformation.is(_first)

  type Second = AnyNaturalTransformation.is[S0]
  val second: Second = AnyNaturalTransformation.is(_second)
}

trait AnyHorizontalComposition extends AnyNaturalTransformation { horiz =>

  // I think I need to have the 4 functors as explicit types here
  type C1 <: AnyCategory;
  type C2 <: AnyCategory;
  type C3 <: AnyCategory
  val c3: AnyCategory.is[C3]

  type F1 <: C1 ⟶ C2
  type F2 <: C2 ⟶ C3
  val F2: AnyFunctor.is[F2]

  type G1 <: C1 ⟶ C2
  type G2 <: C2 ⟶ C3

  type First <: AnyNaturalTransformation {

    type SourceCat = C1; type TargetCat = C2
    type SourceF = F1;
    type TargetF = G1;
  }
  val first: AnyNaturalTransformation.is[First]

  type Second <: AnyNaturalTransformation {

    type SourceCat = C2; type TargetCat = C3;
    type SourceF = F2;
    type TargetF = G2;
  }
  val second: AnyNaturalTransformation.is[Second]

  type SourceCat = C1
  type TargetCat = C3

  type SourceF = F1 >=> F2
  type TargetF = G1 >=> G2

  def at[X <: SourceCat#Objects]: TargetCat#C[SourceF#F[X], TargetF#F[X]] = {

    c3.compose( second.at, F2(first.at) )
  }
}
