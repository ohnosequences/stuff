package ohnosequences.stuff

trait AnyLaxMonoidalFunctor extends AnyFunctor {

  type TargetM <: AnyMonoidalCategory
  val sourceM: SourceM
  type SourceM <: AnyMonoidalCategory
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
  SM <: AnyMonoidalCategory,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyMonoidalCategory
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

  type TargetM <: AnyMonoidalCategory
  val sourceM: SourceM
  type SourceM <: AnyMonoidalCategory
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
  SM <: AnyMonoidalCategory,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyMonoidalCategory
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
  SM <: AnyProducts,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyProducts
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
    AnyMonoidalCategory.is(targetM).univ(
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceM).left[A,B]),
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceM).right[A,B])
    )

  def counit: Target#C[F[SourceM#I], TargetM#I] =
    AnyMonoidalCategory.is(targetM).erase
}

/*
  The co-dual of the above.
*/
case class LaxCocartesianMonoidalFunctor[
  SM <: AnyCoproducts,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyCoproducts
](
  val sourceM: SM,
  val functor: Functor0,
  val targetM: TM
)
extends AnyLaxMonoidalFunctor {

  type SourceM = SM
  type Functor = Functor0
  type TargetM = TM

  def zip[A <: Source#Objects, B <: Source#Objects]: Target#C[F[A] ⋄ F[B], F[A □ B]] =
    AnyMonoidalCategory.is(targetM).univ(
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceM).left[A,B]),
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceM).right[A,B])
    )

  def unit: Target#C[TargetM#I, F[SourceM#I]] =
    AnyMonoidalCategory.is(targetM).nothing

}
