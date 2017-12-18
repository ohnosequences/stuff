package ohnosequences.stuff

import Tuple.{πL, πR}

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
}

object Category {

  @inline
  final def apply[Cat <: Category](cat: is[Cat]): Syntax[Cat] =
    new Syntax(cat)

  final class Syntax[Cat <: Category](val cat: is[Cat]) {

    @inline
    implicit final val _cat: Category.is[Cat] =
      cat

    @inline
    implicit final def morphismSyntax[X <: Cat#Objects, Y <: Cat#Objects](
        f: Cat#C[X, Y]): Category.MorphismSyntax[Cat, X, Y] =
      new MorphismSyntax(f)

    /** @group syntax */
    @infix
    type >=>[A <: Cat#Objects, B <: Cat#Objects] =
      Cat#C[A, B]

    /** @group syntax */
    @inline
    final def id[X <: Cat#Objects]: X >=> X =
      cat.identity[X]
  }

  final class MorphismSyntax[
      Cat <: Category,
      X <: Cat#Objects,
      Y <: Cat#Objects
  ](val f: is[Cat]#C[X, Y])
      extends CompileTime {

    @inline
    final def >=>[Z <: Cat#Objects](g: Cat#C[Y, Z])(
        implicit cat: Category.is[Cat]
    ): Cat#C[X, Z] =
      cat.composition(f and g)

    @inline
    final def ∘[U <: Cat#Objects](h: Cat#C[U, X])(
        implicit cat: Category.is[Cat]
    ): Cat#C[U, Y] =
      cat.composition(h and f)
  }

  type is[category <: Category] =
    category {
      type Objects                                         = category#Objects
      type C[X <: category#Objects, Y <: category#Objects] = category#C[X, Y]
    }

  //////////////////////////////////////////////////////////////////////////////
  // Opposite categories
  //////////////////////////////////////////////////////////////////////////////

  type Op[Cat <: Category] =
    OpImpl {

      type Objects =
        Cat#Objects

      type C[X <: Objects, Y <: Objects] =
        Cat#C[Y, X]
    }

  def op[Cat <: Category]: is[Cat] -> is[Op[Cat]] = { cat =>
    new OpImpl {

      type Objects =
        Cat#Objects

      type C[X <: Objects, Y <: Objects] =
        Cat#C[Y, X]

      @inline
      final def identity[X <: Objects]: C[X, X] =
        cat.identity

      @inline
      final def composition[X <: Objects, Y <: Objects, Z <: Objects]
        : C[X, Y] × C[Y, Z] -> C[X, Z] =
        Product(tuples) ⊢ { swap >-> cat.composition }
    }
  }

  sealed abstract class OpImpl extends Category

  //////////////////////////////////////////////////////////////////////////////
  // Product categories
  //////////////////////////////////////////////////////////////////////////////

  type Unit =
    UnitImpl.type

  @inline
  final def unit: is[Unit] =
    UnitImpl

  object UnitImpl extends Category {

    type Objects                       = ∗
    type C[X <: Objects, Y <: Objects] = ∗

    @inline
    final def identity[X <: Objects]: C[X, X] =
      ∗

    @inline
    final def composition[X <: Objects, Y <: Objects, Z <: Objects]
      : C[X, Y] × C[Y, Z] -> C[X, Z] =
      Product(tuples) ⊢ { erase }
  }

  type Product[LeftCat <: Category, RightCat <: Category] =
    ProductImpl {

      type Objects =
        Tuple {
          type Left <: LeftCat#Objects
          type Right <: RightCat#Objects
        }

      type C[X <: Objects, Y <: Objects] =
        LeftCat#C[X#Left, Y#Left] × RightCat#C[X#Right, Y#Right]
    }

  final def product[LeftCat <: Category, RightCat <: Category]
    : (is[LeftCat] × is[RightCat]) -> is[Product[LeftCat, RightCat]] = { lr =>
    new ProductImpl {

      type Objects =
        Tuple {
          type Left <: LeftCat#Objects
          type Right <: RightCat#Objects
        }

      type C[X <: Objects, Y <: Objects] =
        LeftCat#C[X#Left, Y#Left] × RightCat#C[X#Right, Y#Right]

      val leftCat =
        lr.left

      val rightCat =
        lr.right

      final def identity[X <: Objects]: C[X, X] =
        leftCat.identity and rightCat.identity

      final def composition[X <: Objects, Y <: Objects, Z <: Objects]
        : C[X, Y] × C[Y, Z] -> C[X, Z] =
        ohnosequences.stuff.Product(tuples) ⊢ {
          πL[C[X, Y]] × πL[C[Y, Z]] >-> leftCat.composition ^
            πR[C[X, Y]] × πR[C[Y, Z]] >-> rightCat.composition
        }
    }
  }

  sealed abstract class ProductImpl extends Category

  //////////////////////////////////////////////////////////////////////////////
  // Hom functors
  //////////////////////////////////////////////////////////////////////////////

  type HomFunctor[Cat <: Category] =
    HomFunctorImpl {

      type Source =
        Product[Op[Cat], Cat]

      type Target =
        Scala

      type F[Z <: Source#Objects] =
        Cat#C[Z#Left, Z#Right]
    }

  final def homFunctor[Cat <: Category]
    : is[Cat] -> Functor.is[HomFunctor[Cat]] = { cat =>
    new HomFunctorImpl {

      type Source =
        Product[Op[Cat], Cat]

      type Target =
        Scala

      type F[Z <: Source#Objects] =
        Cat#C[Z#Left, Z#Right]

      val source =
        product(op(cat) and cat)

      val target =
        Scala

      final def at[X <: Source#Objects, Y <: Source#Objects]
        : Source#C[X, Y] -> (F[X] -> F[Y]) = { fg =>
        { q =>
          Product(tuples) ⊢ {
            // left(fg) >=> q >=> right(fg)
            Category(cat) ⊢ { left(fg) >=> q >=> right(fg) }
          }
        }
      }
    }
  }

  sealed abstract class HomFunctorImpl extends Functor
}
