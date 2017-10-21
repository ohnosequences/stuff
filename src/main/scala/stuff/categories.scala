package ohnosequences.stuff

import products._
import functions._

/** Categories

  This encoding is close in spirit to a category enriched in [[Scala]].

  @groupprio syntax 0
  @groupname syntax Syntax
  @groupdesc These are aliases and infix operators (...)
  */
abstract class Category {

  /**
    Acts as the 2-type of types which are objects of this category.

    Categories where objects are encoded in terms of typeclasses should create a type alias/tag or a class storing the corresponding typeclass, and use that as their `Objects` type.
    */
  type Objects

  /** The type of morphisms between `X` and `Y`. */
  type C[X <: Objects, Y <: Objects]

  /** Identity morphism on `X`. */
  def identity[X <: Objects]: C[X, X]

  /** Morphism composition. */
  def composition[X <: Objects, Y <: Objects, Z <: Objects]
    : C[X, Y] × C[Y, Z] -> C[X, Z]

  /** @group syntax */
  @inline
  implicit final val _this: this.type =
    this

  /** @group syntax */
  @infix
  type >=>[A <: Objects, B <: Objects] =
    C[A, B]

  /** @group syntax */
  @inline
  implicit final def morphismSyntax[X <: Objects, Y <: Objects](
      f: C[X, Y]): Category.MorphismSyntax[this.type, X, Y] =
    new Category.MorphismSyntax(f)

  /** @group syntax */
  @inline
  final def id[X <: Objects]: C[X, X] =
    identity[X]
}

object Category {

  final class MorphismSyntax[
      Cat <: Category,
      X <: Cat#Objects,
      Y <: Cat#Objects
  ](val f: is[Cat]#C[X, Y])
      extends scala.AnyVal {

    @inline
    final def >=>[Z <: Cat#Objects](g: Cat#C[Y, Z])(
        implicit cat: Category.is[Cat]
    ): Cat#C[X, Z] =
      cat.composition at (f and g)

    @inline final def ∘[U <: Cat#Objects](h: Cat#C[U, X])(
        implicit cat: Category.is[Cat]
    ): Cat#C[U, Y] =
      cat.composition at (h and f)
  }

  type is[category <: Category] =
    category {
      type Objects                                         = category#Objects
      type C[X <: category#Objects, Y <: category#Objects] = category#C[X, Y]
    }

  final class Opposite[Cat <: Category](val cat: is[Cat]) extends Category {

    final type Objects =
      Cat#Objects

    final type C[X <: Objects, Y <: Objects] =
      Cat#C[Y, X]

    final def identity[X <: Objects]: C[X, X] =
      cat.identity

    final def composition[X <: Objects, Y <: Objects, Z <: Objects]
      : C[X, Y] × C[Y, Z] -> C[X, Z] =
      swap >-> cat.composition
  }

  final def opposite[Cat <: Category]: is[Cat] -> is[Opposite[Cat]] =
    λ { new Opposite(_).asInstanceOf[is[Opposite[Cat]]] }

  type UnitCategory = UnitCategory.type
  object UnitCategory extends Category {

    type Objects                       = ∗
    type C[X <: Objects, Y <: Objects] = ∗

    final def identity[X <: Objects]: C[X, X] =
      ∗

    final def composition[X <: Objects, Y <: Objects, Z <: Objects]
      : C[X, Y] × C[Y, Z] -> C[X, Z] =
      products.erase
  }

  // etc etc
  def lunit[Cat <: Category]: is[Cat] -> Functor =
    λ { cat =>
      new Functor {

        type Source = ProductCategory[UnitCategory, Cat]
        val source = product(UnitCategory and cat)

        type Target = Cat
        val target = cat

        type F[Z <: Source#Objects] = Z#Right

        final def at[X <: Source#Objects, Y <: Source#Objects]
          : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
          right
      }
    }

  final class ProductCategory[
      LeftCat <: Category,
      RightCat <: Category
  ](
      val leftCat: is[LeftCat],
      val rightCat: is[RightCat]
  ) extends Category {

    final type Objects =
      Tuple {
        type Left <: LeftCat#Objects
        type Right <: RightCat#Objects
      }

    final type C[X <: Objects, Y <: Objects] =
      LeftCat#C[X#Left, Y#Left] × RightCat#C[X#Right, Y#Right]

    final def identity[X <: Objects]: C[X, X] =
      leftCat.identity and rightCat.identity

    final def composition[X <: Objects, Y <: Objects, Z <: Objects]
      : C[X, Y] × C[Y, Z] -> C[X, Z] =
      both(
        πL[C[X, Y]] × πL[C[Y, Z]] >-> leftCat.composition and
          πR[C[X, Y]] × πR[C[Y, Z]] >-> rightCat.composition
      )
  }

  final def product[LeftCat <: Category, RightCat <: Category]
    : (is[LeftCat] × is[RightCat]) -> is[ProductCategory[LeftCat, RightCat]] =
    λ { lr =>
      new ProductCategory(left(lr), right(lr))
        .asInstanceOf[is[ProductCategory[LeftCat, RightCat]]]
    }

  final class HomFunctor[Cat <: Category](val cat: is[Cat]) extends Functor {

    type Source = ProductCategory[Opposite[Cat], Cat]
    val source = product(opposite(cat) and cat)
    type Target = Scala
    val target = Scala
    type F[Z <: Source#Objects] = Cat#C[Z#Left, Z#Right]

    final def at[X <: SourceObjects, Y <: SourceObjects]
      : Source#C[X, Y] -> (F[X] -> F[Y]) =
      λ { fg =>
        λ { q =>
          cat.composition(cat.composition(left(fg) and q) and right(fg))
        }
      }
  }

  final def homFunctor[Cat <: Category]
    : is[Cat] -> Functor.is[HomFunctor[Cat]] =
    λ { new HomFunctor(_).asInstanceOf[Functor.is[HomFunctor[Cat]]] }
}
