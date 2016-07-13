package ohnosequences.stuff

trait AnyKleisliCoproductStructure extends AnyCocartesianMonoidalStructure { kleisliSums =>

  type BaseCocartesian <: AnyCocartesianMonoidalStructure
  val baseCocartesian: BaseCocartesian

  type On <: AnyKleisliCategory {
    type Cat      = BaseCocartesian#On
    type Functor  = kleisliSums.Functor
    type Monad    = kleisliSums.Monad
  }

  type Functor <: BaseCocartesian#On ⟶ BaseCocartesian#On

  type Monad <: AnyMonad {
    type On       = BaseCocartesian#On
    type Functor  = kleisliSums.Functor
  }

  lazy val Free = on.freeF
  lazy val trueBase = AnyCocartesianMonoidalStructure.is(baseCocartesian)

  type ⊗[A <: On#Objects, B <: On#Objects] = BaseCocartesian# ⊗[A,B]
  type I = BaseCocartesian#I

  def left[A <: On#Objects, B <: On#Objects]: BaseCocartesian#On#C[A, Functor#F[A + B]] =
    Free(trueBase.left)

  def right[A <: On#Objects, B <: On#Objects]: BaseCocartesian#On#C[B, Functor#F[A + B]] =
    Free(trueBase.right)

  def nothing[A <: On#Objects]: BaseCocartesian#On#C[I, Functor#F[A]] =
    Free(trueBase.nothing)

  def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]
  : ( BaseCocartesian#On#C[A, Functor#F[X]], BaseCocartesian#On#C[B, Functor#F[X]] ) =>
    BaseCocartesian#On#C[(A + B), Functor#F[X]] =
    (a,b) => trueBase.univ(a, b)

  def assoc_left[A <: On#Objects, B <: On#Objects, C <: On#Objects]: BaseCocartesian#On#C[ A ⊗ (B ⊗ C), Functor#F[(A ⊗ B) ⊗ C] ] =
    Free(trueBase.assoc_left)

  def assoc_right[A <: On#Objects, B <: On#Objects, C <: On#Objects]: BaseCocartesian#On#C[ (A ⊗ B) ⊗ C, Functor#F[A ⊗ (B ⊗ C)] ] =
    Free(trueBase.assoc_right)
}

case class KleisliCoproductStructure[
  On0 <: AnyCategory,
  BaseCocartesian0 <: AnyCocartesianMonoidalStructure { type On = On0 },
  Functor0 <: AnyFunctor { type Source = On0; type Target = On0 },
  Monad0 <: AnyMonad { type On = On0; type Functor = Functor0 },
  KlC0 <: AnyKleisliCategory { type Cat = On0; type Functor = Functor0; type Monad = Monad0 }
]
(val on: KlC0, val baseCocartesian: BaseCocartesian0) extends AnyKleisliCoproductStructure {

  type BaseCocartesian  = BaseCocartesian0
  type Functor          = Functor0
  type Monad            = Monad0

  type On = KlC0
}
