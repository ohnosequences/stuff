package ohnosequences.stuff

import functions._
import products._

abstract class MonoidalCategory {

  type On <: Category
  val on: Category.is[On]

  type I <: On#Objects

  @infix
  type ⊗[X <: On#Objects, Y <: On#Objects] <: On#Objects

  def ⊗[A <: On#Objects, B <: On#Objects, C <: On#Objects, D <: On#Objects]
    : (On#C[A, B] × On#C[C, D]) -> On#C[A ⊗ C, B ⊗ D]

  def assoc_right[A <: On#Objects, B <: On#Objects, C <: On#Objects]
    : On#C[(A ⊗ B) ⊗ C, A ⊗ (B ⊗ C)]
  def assoc_left[A <: On#Objects, B <: On#Objects, C <: On#Objects]
    : On#C[A ⊗ (B ⊗ C), (A ⊗ B) ⊗ C]

  def unitl[A <: On#Objects]: On#C[I ⊗ A, A]
  def unitr[A <: On#Objects]: On#C[A ⊗ I, A]
}

object MonoidalCategory {

  class LeftTensor[MCat <: MonoidalCategory, A <: MCat#On#Objects](
      val mcat: is[MCat])
      extends Functor {

    type Source = MCat#On
    val source = mcat.on

    type Target = MCat#On
    val target = mcat.on

    type F[X <: Source#Objects] =
      // format: off
      MCat # ⊗[A, X]
      // format: on

    def at[X <: Source#Objects, Y <: Source#Objects]
      : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
      λ { f: Source#C[X, Y] =>
        mcat.⊗(mcat.on.identity and f)
      }
  }

  class RightTensor[MCat <: MonoidalCategory, A <: MCat#On#Objects](
      val mcat: is[MCat])
      extends Functor {

    type Source = MCat#On
    val source = mcat.on

    type Target = MCat#On
    val target = mcat.on

    type F[X <: Source#Objects] =
      // format: off
      MCat # ⊗[X, A]
      // format: on

    def at[X <: Source#Objects, Y <: Source#Objects]
      : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
      λ { f: Source#C[X, Y] =>
        mcat ⊗ (f and mcat.on.identity)
      }
  }

  class TensorFunctor[MCat <: MonoidalCategory](val mcat: is[MCat])
      extends Functor {

    type Source = Category.Product[MCat#On, MCat#On]
    val source = Category.product(mcat.on, mcat.on)

    type Target = MCat#On
    val target = mcat.on

    type F[X <: Source#Objects] =
      // format: off
      MCat # ⊗[X#Left, X#Right]
      // format: on

    def at[X <: Source#Objects, Y <: Source#Objects]
      : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
      mcat ⊗
  }

  type is[MCat <: MonoidalCategory] =
    MCat {
      type On = MCat#On
      type I  = MCat#I
      // format: off
      type ⊗[X <: On#Objects, Y <: On#Objects] = MCat # ⊗[X, Y]
      // format: on
    }
}

abstract class SymmetricStructure {

  type On <: MonoidalCategory
  val on: MonoidalCategory.is[On]

  def swap[X <: On#On#Objects, Y <: On#On#Objects]
  // format: off
    : On#On#C[On # ⊗[X, Y], On # ⊗[Y, X]]
    // format: on
}
