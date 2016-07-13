package ohnosequences.stuff

trait AnyKleisliCoproducts extends AnyCoproducts { kleisliCoproducts =>

  type BaseCoproducts <: AnyCoproducts
  val baseCoproducts: BaseCoproducts

  type On <: AnyKleisliCategory {
    type Cat      = BaseCoproducts#On
    type Functor  = kleisliCoproducts.Functor
    type Monad    = kleisliCoproducts.Monad
  }

  type Functor <: BaseCoproducts#On ⟶ BaseCoproducts#On

  type Monad <: AnyMonad {
    type On       = BaseCoproducts#On
    type Functor  = kleisliCoproducts.Functor
  }

  lazy val Free = on.freeF
  lazy val trueBase = AnyCoproducts.is(baseCoproducts)

  type ⊗[A <: On#Objects, B <: On#Objects] = BaseCoproducts# ⊗[A,B]
  type I = BaseCoproducts#I

  def left[A <: On#Objects, B <: On#Objects]: BaseCoproducts#On#C[A, Functor#F[A + B]] =
    Free(trueBase.left)

  def right[A <: On#Objects, B <: On#Objects]: BaseCoproducts#On#C[B, Functor#F[A + B]] =
    Free(trueBase.right)

  def nothing[A <: On#Objects]: BaseCoproducts#On#C[I, Functor#F[A]] =
    Free(trueBase.nothing)

  def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]
  : ( BaseCoproducts#On#C[A, Functor#F[X]], BaseCoproducts#On#C[B, Functor#F[X]] ) =>
    BaseCoproducts#On#C[(A + B), Functor#F[X]] =
    (a,b) => trueBase.univ(a, b)

  def assoc_left[A <: On#Objects, B <: On#Objects, C <: On#Objects]: BaseCoproducts#On#C[ A ⊗ (B ⊗ C), Functor#F[(A ⊗ B) ⊗ C] ] =
    Free(trueBase.assoc_left)

  def assoc_right[A <: On#Objects, B <: On#Objects, C <: On#Objects]: BaseCoproducts#On#C[ (A ⊗ B) ⊗ C, Functor#F[A ⊗ (B ⊗ C)] ] =
    Free(trueBase.assoc_right)
}

case class KleisliCoproducts[
  On0 <: AnyCategory,
  BaseCoproducts0 <: AnyCoproducts { type On = On0 },
  Functor0 <: AnyFunctor { type Source = On0; type Target = On0 },
  Monad0 <: AnyMonad { type On = On0; type Functor = Functor0 },
  KlC0 <: AnyKleisliCategory { type Cat = On0; type Functor = Functor0; type Monad = Monad0 }
]
(val on: KlC0, val baseCoproducts: BaseCoproducts0) extends AnyKleisliCoproducts {

  type BaseCoproducts  = BaseCoproducts0
  type Functor          = Functor0
  type Monad            = Monad0

  type On = KlC0
}
