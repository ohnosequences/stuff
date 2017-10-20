package ohnosequences.stuff

import products._
import functions._

/** Categories

  This encoding is close in spirit to a category enriched in [[Scala]].
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
}

object Category {

  type is[category <: Category] =
    category {
      type Objects                                         = category#Objects
      type C[X <: category#Objects, Y <: category#Objects] = category#C[X, Y]
    }

  type Opposite[category <: Category] =
    Category {
      type Objects                       = category#Objects
      type C[X <: Objects, Y <: Objects] = category#C[Y, X]
    }

  final def opposite[category <: Category](
      c: is[category]): Opposite[category] =
    new Category {

      type Objects =
        category#Objects

      type C[X <: Objects, Y <: Objects] =
        category#C[Y, X]

      final def identity[X <: Objects]: C[X, X] =
        c.identity

      final def composition[X <: Objects, Y <: Objects, Z <: Objects]
        : C[X, Y] × C[Y, Z] -> C[X, Z] =
        swap >-> c.composition
    }

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
