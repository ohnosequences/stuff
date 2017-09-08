package ohnosequences.stuff

import functions._
import products._

abstract class MonoidalCategory {

  type On <: Category
  val on: On

  type I <: On#Objects

  @infix
  type ⊗[X <: On#Objects, Y <: On#Objects] <: On#Objects

  def ⊗[A <: On#Objects, B <: On#Objects, C <: On#Objects, D <: On#Objects]:
    (On#C[A,B] × On#C[C,D]) -> On#C[A ⊗ C, B ⊗ D]

  def assoc_right[A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ (A ⊗ B) ⊗ C, A ⊗ (B ⊗ C) ]
  def  assoc_left[A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ A ⊗ (B ⊗ C), (A ⊗ B) ⊗ C ]

  def unitl[A <: On#Objects]: On#C[I ⊗ A, A]
  def unitr[A <: On#Objects]: On#C[A ⊗ I, A]

  /** syntax */
  type Objects = On#Objects

  type Hom[X <: On#Objects, Y <: On#Objects] = On#C[X,Y]

  @inline
  implicit final
  val _on: On =
    on

  @inline
  implicit final
  val _this: this.type =
    this

  @inline
  implicit final
  def morphismSyntax[X <: Objects, Y <: Objects](f: Hom[X,Y])
  : Category.MorphismSyntax[On, X, Y] =
    new Category.MorphismSyntax(f)

  @inline
  final
  def id[X <: Objects]: Hom[X,X] =
    Category.is(on).identity[X]

  @inline
  final
  def ⊗-[A <: On#Objects]: MonoidalCategory.LeftTensor[this.type, A] =
    new MonoidalCategory.LeftTensor(this)

  @inline
  final
  def -⊗[A <: On#Objects]: MonoidalCategory.RightTensor[this.type, A] =
    new MonoidalCategory.RightTensor(this)

  @inline
  implicit final
  def monoidalMorphismSyntax[X <: Objects, Y <: Objects](f: Hom[X,Y])
  : MonoidalCategory.MorphismSyntax[this.type, X, Y] =
    new MonoidalCategory.MorphismSyntax(f)
}

object MonoidalCategory {

  final
  class MorphismSyntax[
    MonCat <: MonoidalCategory,
    A1 <: MonCat#On#Objects,
    B1 <: MonCat#On#Objects
  ]
  (val f: MonCat#On#C[A1,B1]) extends scala.AnyVal {

    @inline
    final
    def ⊗[A2 <: MonCat#On#Objects, B2 <: MonCat#On#Objects](g: MonCat#On#C[A2,B2])(implicit monCat: MonCat): MonCat#On#C[MonCat# ⊗[A1,A2], MonCat# ⊗[B1,B2]] =
      MonoidalCategory.is(monCat). ⊗ (f and g)
  }

  class LeftTensor[MCat <: MonoidalCategory, A <: MCat#On#Objects](val mcat: MCat) extends Functor {

    type Source = MCat#On
    val source  = mcat.on

    type Target = MCat#On
    val target  = mcat.on

    type F[X <: Source#Objects] =
      MCat# ⊗[A,X]

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      is(mcat) ⊢ { λ { id ⊗ _ } }
  }

  class RightTensor[MCat <: MonoidalCategory, A <: MCat#On#Objects](val mcat: MCat) extends Functor {

    type Source = MCat#On
    val source  = mcat.on

    type Target = MCat#On
    val target  = mcat.on

    type F[X <: Source#Objects] =
      MCat# ⊗[X,A]

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      is(mcat) ⊢ { λ {  _ ⊗ id } }
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

abstract class SymmetricStructure {

  type On <: MonoidalCategory
  val on: On

  def swap[X <: On#On#Objects, Y <: On#On#Objects]: On#On#C[On# ⊗[X,Y], On# ⊗[Y,X]]
}
