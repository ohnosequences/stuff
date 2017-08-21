package ohnosequences.stuff

import functions._
import products._

abstract class MonoidalCategory {

  type On <: Category
  val on: On

  type I <: On#Objects
  type ⊗[X <: On#Objects, Y <: On#Objects] <: On#Objects

  def ⊗[A <: On#Objects, B <: On#Objects, C <: On#Objects, D <: On#Objects]:
    (On#C[A,B] × On#C[C,D]) -> On#C[A ⊗ C, B ⊗ D]

  def assoc_right[A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ (A ⊗ B) ⊗ C, A ⊗ (B ⊗ C) ]
  def  assoc_left[A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ A ⊗ (B ⊗ C), (A ⊗ B) ⊗ C ]

  def unitl[A <: On#Objects]: On#C[I ⊗ A, A]
  def unitr[A <: On#Objects]: On#C[A ⊗ I, A]
}

object MonoidalCategory {

  class LeftTensor[MCat <: MonoidalCategory, A <: MCat#On#Objects](val mcat: MCat) extends Functor {

    type Source = MCat#On
    val source  = mcat.on

    type Target = MCat#On
    val target  = mcat.on

    type F[X <: Source#Objects] =
      MCat# ⊗[A,X]

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      λ { f: Source#C[X,Y] => is(mcat).⊗(Category.is(is(mcat).on).identity and f) }
  }

  class RightTensor[MCat <: MonoidalCategory, A <: MCat#On#Objects](val mcat: MCat) extends Functor {

    type Source = MCat#On
    val source  = mcat.on

    type Target = MCat#On
    val target  = mcat.on

    type F[X <: Source#Objects] =
      MCat# ⊗[X,A]

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      λ { f: Source#C[X,Y] => is(mcat).⊗(f and Category.is(is(mcat).on).identity) }
  }

  class TensorFunctor[MCat <: MonoidalCategory](val mcat: MCat) extends Functor {

    type Source = Category.Product[MCat#On, MCat#On]
    val source  = Category.product(mcat.on, mcat.on)

    type Target = MCat#On
    val target  = mcat.on

    type F[X <: Source#Objects] =
      MCat# ⊗[X#Left,X#Right]

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      is(mcat).⊗
  }

  def is[MCat <: MonoidalCategory](mcat: MCat): is[MCat] =
    mcat.asInstanceOf[is[MCat]]

  type is[MCat <: MonoidalCategory] =
    MCat {
      type On = MCat#On
      type I = MCat#I
      type ⊗[X <: On#Objects, Y <: On#Objects] = MCat# ⊗[X,Y]
    }
}
