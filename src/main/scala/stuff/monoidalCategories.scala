package ohnosequences.stuff

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

  @inline final def apply[MonCat <: MonoidalCategory](
      monCat: is[MonCat]): Syntax[MonCat] =
    new Syntax(monCat)

  final class Syntax[MonCat <: MonoidalCategory](val monCat: is[MonCat]) {

    type Objects = MonCat#On#Objects

    @inline implicit final val _on: Category.is[MonCat#On] =
      monCat.on

    @inline implicit final val _monCat: is[MonCat] =
      monCat

    @inline implicit final def categorySyntax[X <: Objects, Y <: Objects](
        f: MonCat#On#C[X, Y]): Category.MorphismSyntax[MonCat#On, X, Y] =
      new Category.MorphismSyntax[MonCat#On, X, Y](f)

    @inline final def id[X <: Objects]: MonCat#On#C[X, X] =
      monCat.on.identity[X]

    @inline
    final def ⊗-[A <: Objects]: LeftTensor[MonCat, A] =
      new LeftTensor(monCat)

    @inline
    final def -⊗[A <: Objects]: RightTensor[MonCat, A] =
      new RightTensor(monCat)

    @inline implicit final def syntax[X <: Objects, Y <: Objects](
        f: MonCat#On#C[X, Y]): MorphismSyntax[MonCat, X, Y] =
      new MorphismSyntax(f)
  }

  final class MorphismSyntax[
      MonCat <: MonoidalCategory,
      A1 <: MonCat#On#Objects,
      B1 <: MonCat#On#Objects
  ](val f: MonCat#On#C[A1, B1])
      extends scala.AnyVal {

    @inline
    final def ⊗[
        A2 <: MonCat#On#Objects,
        B2 <: MonCat#On#Objects
    ](g: MonCat#On#C[A2, B2])(
        implicit monCat: MonoidalCategory.is[MonCat] // format: off
    ): MonCat#On#C[MonCat# ⊗[A1, A2], MonCat# ⊗[B1, B2]] =  // format: on
    monCat ⊗ (f and g)
  }

  final class LeftTensor[MCat <: MonoidalCategory, A <: MCat#On#Objects](
      val mcat: is[MCat])
      extends Functor {

    type Source = MCat#On
    val source = mcat.on

    type Target = MCat#On
    val target = mcat.on

    type F[X <: Source#Objects] = // format: off
      MCat# ⊗[A, X]               // format: on

    def at[X <: Source#Objects, Y <: Source#Objects]
      : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
      MonoidalCategory(mcat) ⊢ { λ { id ⊗ _ } }
  }

  final class RightTensor[MCat <: MonoidalCategory, A <: MCat#On#Objects](
      val mcat: is[MCat])
      extends Functor {

    type Source = MCat#On
    val source = mcat.on

    type Target = MCat#On
    val target = mcat.on

    type F[X <: Source#Objects] = // format: off
      MCat# ⊗[X, A]               // format: on

    def at[X <: Source#Objects, Y <: Source#Objects]
      : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
      MonoidalCategory(mcat) ⊢ { λ { _ ⊗ id } }
  }

  class TensorFunctor[MCat <: MonoidalCategory](val mcat: is[MCat])
      extends Functor {

    type Source = Category.ProductCategory[MCat#On, MCat#On]
    val source = Category.product(mcat.on and mcat.on)

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

  type inferIs[MCat <: MonoidalCategory] >: is[MCat] <: is[MCat]
}

abstract class SymmetricStructure {

  type On <: MonoidalCategory
  val on: MonoidalCategory.is[On]

  def swap[X <: On#On#Objects, Y <: On#On#Objects]
  // format: off
    : On#On#C[On # ⊗[X, Y], On # ⊗[Y, X]]
    // format: on
}
