package ohnosequences.stuff

trait AnyLaxMonoidalFunctor {

  type SourceMonoidalCategory <: AnyMonoidalCategory
  val targetMonoidalCategory: TargetMonoidalCategory

  type TargetMonoidalCategory <: AnyMonoidalCategory
  val sourceMonoidalCategory: SourceMonoidalCategory

  // NOTE notation
  type □[X <: Source#Objects, Y <: Source#Objects] = SourceMonoidalCategory# ⊗[X,Y]
  type ⋄[X <: Target#Objects, Y <: Target#Objects] = TargetMonoidalCategory# ⊗[X,Y]

  type Source = SourceMonoidalCategory#On
  lazy val source = sourceMonoidalCategory.on
  type Target = TargetMonoidalCategory#On
  lazy val target = targetMonoidalCategory.on

  type Functor <: AnyFunctor {
    type Source = SourceMonoidalCategory#On
    type Target = TargetMonoidalCategory#On
  }
  val functor: Functor

  def zip[A <: Source#Objects, B <: Source#Objects]: Target#C[Functor#F[A] ⋄ Functor#F[B], Functor#F[A □ B]]

  def unit: Target#C[TargetMonoidalCategory#I, Functor#F[SourceMonoidalCategory#I]]
}

abstract class LaxMonoidalFunctor[
  SM <: AnyMonoidalCategory,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyMonoidalCategory
]
(
  val sourceMonoidalCategory: SM,
  val functor: Functor0,
  val targetMonoidalCategory: TM
)
extends AnyLaxMonoidalFunctor {

  type SourceMonoidalCategory = SM
  type Functor = Functor0
  type TargetMonoidalCategory = TM
}

trait AnyColaxMonoidalFunctor {

  type TargetMonoidalCategory <: AnyMonoidalCategory
  val sourceMonoidalCategory: SourceMonoidalCategory
  type SourceMonoidalCategory <: AnyMonoidalCategory
  val targetMonoidalCategory: TargetMonoidalCategory

  // NOTE notation
  type □[X <: Source#Objects, Y <: Source#Objects] = SourceMonoidalCategory# ⊗[X,Y]
  type ⋄[X <: Target#Objects, Y <: Target#Objects] = TargetMonoidalCategory# ⊗[X,Y]

  type Source = SourceMonoidalCategory#On
  lazy val source = sourceMonoidalCategory.on
  type Target = TargetMonoidalCategory#On
  lazy val target = targetMonoidalCategory.on

  type Functor <: AnyFunctor {
    type Source = SourceMonoidalCategory#On
    type Target = TargetMonoidalCategory#On
  }
  val functor: Functor

  def unzip[A <: Source#Objects, B <: Source#Objects]: Target#C[Functor#F[A □ B], Functor#F[A] ⋄ Functor#F[B]]

  def counit: Target#C[Functor#F[SourceMonoidalCategory#I], TargetMonoidalCategory#I]
}

abstract class ColaxMonoidalFunctor[
  SM <: AnyMonoidalCategory,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyMonoidalCategory
]
(
  val sourceMonoidalCategory: SM,
  val functor: Functor0,
  val targetMonoidalCategory: TM
)
extends AnyColaxMonoidalFunctor {

  type SourceMonoidalCategory = SM
  type Functor = Functor0
  type TargetMonoidalCategory = TM
}

/*
  A functor between cartesian monoidal categories is automatically colax monoidal. This construction only requires of the domain to be *affine* (a terminal unit), but we don't want to make this overly complex. See for example the [nLab](https://ncatlab.org/nlab/show/semicartesian+monoidal+category).
*/
case class ColaxCartesianMonoidalFunctor[
  SM <: AnyProducts,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyProducts
](
  val sourceMonoidalCategory: SM,
  val functor: Functor0,
  val targetMonoidalCategory: TM
)
extends AnyColaxMonoidalFunctor {

  type SourceMonoidalCategory = SM
  type Functor = Functor0
  type TargetMonoidalCategory = TM

  def unzip[A <: Source#Objects, B <: Source#Objects]: Target#C[Functor#F[A □ B], Functor#F[A] ⋄ Functor#F[B]] =
    AnyMonoidalCategory.is(targetMonoidalCategory).univ(
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceMonoidalCategory).left[A,B]),
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceMonoidalCategory).right[A,B])
    )

  def counit: Target#C[Functor#F[SourceMonoidalCategory#I], TargetMonoidalCategory#I] =
    AnyMonoidalCategory.is(targetMonoidalCategory).erase
}

/*
  The co-dual of the above.
*/
case class LaxCocartesianMonoidalFunctor[
  SM <: AnyCoproducts,
  Functor0 <: AnyFunctor { type Source = SM#On; type Target = TM#On },
  TM <: AnyCoproducts
](
  val sourceMonoidalCategory: SM,
  val functor: Functor0,
  val targetMonoidalCategory: TM
)
extends AnyLaxMonoidalFunctor {

  type SourceMonoidalCategory = SM
  type Functor = Functor0
  type TargetMonoidalCategory = TM

  def zip[A <: Source#Objects, B <: Source#Objects]: Target#C[Functor#F[A] ⋄ Functor#F[B], Functor#F[A □ B]] =
    AnyMonoidalCategory.is(targetMonoidalCategory).univ(
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceMonoidalCategory).left[A,B]),
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceMonoidalCategory).right[A,B])
    )

  def unit: Target#C[TargetMonoidalCategory#I, Functor#F[SourceMonoidalCategory#I]] =
    AnyMonoidalCategory.is(targetMonoidalCategory).nothing

}
