package ohnosequences.stuff

trait AnyKleisliCoproductStructure extends AnyCocartesianMonoidalStructure { kleisliSums =>

  type BaseCocartesian <: AnyCocartesianMonoidalStructure
  val baseCocartesian: BaseCocartesian
  type On <: AnyKleisliCategory {
    type Cat = BaseCocartesian#On
    type Objects = BaseCocartesian#On#Objects
    type Functor = kleisliSums.Functor
    type Monad = kleisliSums.Monad
  }

  type Functor <: BaseCocartesian#On ⟶ BaseCocartesian#On
  type Monad <: AnyMonad { type Functor = kleisliSums.Functor; type On = BaseCocartesian#On }

  lazy val Free = on.freeF
  lazy val trueBase = AnyCocartesianMonoidalStructure.is(baseCocartesian)

  type ⊗[A <: On#Objects, B <: On#Objects] = BaseCocartesian# ⊗[A,B]
  type I = BaseCocartesian#I

  override def left[A <: On#Objects, B <: On#Objects]: BaseCocartesian#On#C[A, Functor#F[A + B]] =
    Free(trueBase.left)

  override def right[A <: On#Objects, B <: On#Objects]: BaseCocartesian#On#C[B, Functor#F[A + B]] =
    Free(trueBase.right)

  override def nothing[A <: On#Objects]: BaseCocartesian#On#C[I, Functor#F[A]] =
    Free(trueBase.nothing)

  override def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]
  : ( BaseCocartesian#On#C[A, Functor#F[X]], BaseCocartesian#On#C[B, Functor#F[X]] ) =>
    BaseCocartesian#On#C[(A + B), Functor#F[X]] =
    (a,b) => trueBase.univ(a, b)
}

case class KleisliCoproductStructure[
  On0 <: AnyCategory,
  BaseCocartesian0 <: AnyCocartesianMonoidalStructure { type On = On0 },
  Functor0 <: AnyFunctor { type Source = On0; type Target = On0 },
  Monad0 <: AnyMonad { type On = On0; type Functor = Functor0 }
](val monad: Monad0, val baseCocartesian: BaseCocartesian0) extends AnyKleisliCoproductStructure {

  type BaseCocartesian = BaseCocartesian0
  type Functor = Functor0
  type Monad = Monad0
  type On = KleisliCategory[On0,Functor0,Monad0]
  lazy val on = KleisliCategory[On0,Functor0,Monad0](monad)

  // TODO clean this
  def assoc_left[A <: On0#Objects, B <: On0#Objects, C <: On0#Objects]: On0#C[BaseCocartesian0# ⊗[A,BaseCocartesian0# ⊗[B,C]],Functor0#F[BaseCocartesian0# ⊗[BaseCocartesian0# ⊗[A,B],C]]] = ???

  def assoc_right[A <: On0#Objects, B <: On0#Objects, C <: On0#Objects]: On0#C[BaseCocartesian0# ⊗[BaseCocartesian0# ⊗[A,B],C],Functor0#F[BaseCocartesian0# ⊗[A,BaseCocartesian0# ⊗[B,C]]]] = ???

}
