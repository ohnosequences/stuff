package ohnosequences.stuff

trait AnyLaxMonoidalFunctor extends AnyFunctor {

  type TargetM <: AnyMonoidalStructure
  val sourceM: SourceM
  type SourceM <: AnyMonoidalStructure
  val targetM: TargetM

  // NOTE notation
  type □[X <: Source#Objects, Y <: Source#Objects] = SourceM# ⊗[X,Y]
  type ⋄[X <: Target#Objects, Y <: Target#Objects] = TargetM# ⊗[X,Y]

  type Source = SourceM#On
  lazy val source = sourceM.on
  type Target = TargetM#On
  lazy val target = targetM.on

  type Functor <: AnyFunctor {
    type Source = SourceM#On
    type Target = TargetM#On
  }
  val functor: Functor

  type F[X <: Source#Objects] = Functor#F[X]

  def apply[A <: Source#Objects, B <: Source#Objects](f: Source#C[A,B]): Target#C[F[A], F[B]] =
    AnyFunctor.is(functor)(f)

  def zip[A <: Source#Objects, B <: Source#Objects]: Target#C[F[A] ⋄ F[B], F[A □ B]]

  def unit: Target#C[TargetM#I, F[SourceM#I]]
}

abstract class LaxMonoidalFunctor[
  SM <: AnyMonoidalStructure,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyMonoidalStructure
]
(
  val sourceM: SM,
  val functor: Functor0,
  val targetM: TM
)
extends AnyLaxMonoidalFunctor {

  type SourceM = SM
  type Functor = Functor0
  type TargetM = TM
}

trait AnyColaxMonoidalFunctor extends AnyFunctor {

  type TargetM <: AnyMonoidalStructure
  val sourceM: SourceM
  type SourceM <: AnyMonoidalStructure
  val targetM: TargetM

  // NOTE notation
  type □[X <: Source#Objects, Y <: Source#Objects] = SourceM# ⊗[X,Y]
  type ⋄[X <: Target#Objects, Y <: Target#Objects] = TargetM# ⊗[X,Y]

  type Source = SourceM#On
  lazy val source = sourceM.on
  type Target = TargetM#On
  lazy val target = targetM.on

  type Functor <: AnyFunctor {
    type Source = SourceM#On
    type Target = TargetM#On
  }
  val functor: Functor

  type F[X <: Source#Objects] = Functor#F[X]

  def apply[A <: Source#Objects, B <: Source#Objects](f: Source#C[A,B]): Target#C[F[A], F[B]] =
    AnyFunctor.is(functor)(f)

  def unzip[A <: Source#Objects, B <: Source#Objects]: Target#C[F[A □ B], F[A] ⋄ F[B]]

  def counit: Target#C[F[SourceM#I], TargetM#I]
}

abstract class ColaxMonoidalFunctor[
  SM <: AnyMonoidalStructure,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyMonoidalStructure
]
(
  val sourceM: SM,
  val functor: Functor0,
  val targetM: TM
)
extends AnyColaxMonoidalFunctor {

  type SourceM = SM
  type Functor = Functor0
  type TargetM = TM
}

/*
  A functor between cartesian monoidal categories is automatically colax monoidal. This construction only requires of the domain to be *affine* (a terminal unit), but we don't want to make this overly complex. See for example the [nLab](https://ncatlab.org/nlab/show/semicartesian+monoidal+category).
*/
case class ColaxCartesianMonoidalFunctor[
  SM <: AnyCartesianMonoidalStructure,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyCartesianMonoidalStructure
](
  val sourceM: SM,
  val functor: Functor0,
  val targetM: TM
)
extends AnyColaxMonoidalFunctor {

  type SourceM = SM
  type Functor = Functor0
  type TargetM = TM

  def unzip[A <: Source#Objects, B <: Source#Objects]: Target#C[F[A □ B], F[A] ⋄ F[B]] =
    AnyMonoidalStructure.is(targetM).univ(
      AnyFunctor.is(functor)(AnyMonoidalStructure.is(sourceM).left[A,B]),
      AnyFunctor.is(functor)(AnyMonoidalStructure.is(sourceM).right[A,B])
    )

  def counit: Target#C[F[SourceM#I], TargetM#I] =
    AnyMonoidalStructure.is(targetM).erase

}
