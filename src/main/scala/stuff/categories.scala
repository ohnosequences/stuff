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
  def identity[X <: Objects]: C[X,X]

  /** Morphism composition. */
  def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z]

  /** @group syntax */
  @inline
  implicit final
  val _this: this.type =
    this

  /** @group syntax */
  @infix
  type >=>[A <: Objects, B <: Objects] =
    C[A,B]

  /** @group syntax */
  @inline
  implicit final
  def morphismSyntax[X <: Objects, Y <: Objects](f: C[X,Y])
  : Category.MorphismSyntax[this.type,X,Y] =
    new Category.MorphismSyntax(f)

  /** @group syntax */
  @inline
  final
  def id[X <: Objects]: C[X,X] =
    identity[X]
}

object Category {

  final
  class MorphismSyntax[
    Cat <: Category,
    X <: Cat#Objects,
    Y <: Cat#Objects
  ]
  (val f: Cat#C[X,Y]) extends scala.AnyVal {

    @inline
    final
    def >=>[Z <: Cat#Objects](g: Cat#C[Y,Z])(implicit cat: Cat): Cat#C[X,Z] =
      Category.is(cat).composition at (f and g)

    @inline
    final
    def ∘[U <: Cat#Objects](h: Cat#C[U,X])(implicit cat: Cat): Cat#C[U,Y] =
      Category.is(cat).composition at (h and f)
  }

  type is[category <: Category] =
    category {
      type Objects = category#Objects
      type C[X <: category#Objects, Y <: category#Objects] = category#C[X,Y]
    }

  final
  def is[category <: Category](c: category): is[category] =
    c.asInstanceOf[is[category]]

  type Opposite[category <: Category] =
    Category {
      type Objects = category#Objects
      type C[X <: Objects, Y <: Objects] = category#C[Y,X]
    }

  final
  def opposite[category <: Category](c: category): Opposite[category] =
    new Category {

      type Objects =
        category#Objects

      type C[X <: Objects, Y <: Objects] =
        category#C[Y,X]

      final
      def identity[X <: Objects]: C[X,X] =
        is(c).identity

      final
      def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
        swap >-> is(c).composition
    }

  type UnitCategory = UnitCategory.type
  object UnitCategory extends Category {

    type Objects = ∗
    type C[X <: Objects, Y <: Objects] = ∗

    final
    def identity[X <: Objects]: C[X,X] =
      ∗

    final
    def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
      products.erase
  }

  // etc etc
  def lunit[Cat <: Category]: Cat -> Functor =
    λ { cat =>
      new Functor {

        type Source = Product[UnitCategory, Cat]
        val source: Source = product(UnitCategory, cat)

        type Target = Cat
        val target: Target = cat

        type F[Z <: Source#Objects] = Z#Right

        final
        def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
          right
      }
    }

  type Product[leftCategory <: Category, rightCategory <: Category] =
    Category {

      type Objects =
        Tuple {
          type Left   <: leftCategory#Objects
          type Right  <: rightCategory#Objects
        }

      type C[X <: Objects, Y <: Objects] =
        leftCategory#C[X#Left,Y#Left] × rightCategory#C[X#Right, Y#Right]
    }

  final
  def product[leftCategory <: Category, rightCategory <: Category](l: leftCategory, r: rightCategory): Product[leftCategory,rightCategory] =
    new Category {

      type Objects =
        Tuple {
          type Left   <: leftCategory#Objects
          type Right  <: rightCategory#Objects
        }

      type C[X <: Objects, Y <: Objects] =
        leftCategory#C[X#Left,Y#Left] × rightCategory#C[X#Right, Y#Right]

      final
      def identity[X <: Objects]: C[X,X] =
        is(l).identity and is(r).identity

      final
      def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
        both(
          πL[C[X,Y]] × πL[C[Y,Z]] >-> is(l).composition and
          πR[C[X,Y]] × πR[C[Y,Z]] >-> is(r).composition
        )
    }

  final
  type Hom[Cat <: Category] =
    Functor {
      type Source = Product[Opposite[Cat], Cat]
      type Target = Scala.type
      type F[Z <: Source#Objects] = Cat#C[Z#Left, Z#Right]
    }

  final
  def hom[Cat <: Category]: Cat -> Hom[Cat] =
    λ { cat: Cat =>

      new Functor {

        type Source = Product[Opposite[Cat], Cat]
        final
        val source = product(opposite(cat), cat)

        type Target = Scala.type
        final
        val target = Scala

        type F[Z <: Source#Objects] = Cat#C[Z#Left, Z#Right]

        final
        def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> (F[X] -> F[Y]) =
          λ { fg =>
            λ { q =>
              is(cat).composition( is(cat).composition(left(fg) and q) and right(fg))
            }
          }
      }
    }
}
