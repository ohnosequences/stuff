package ohnosequences.stuff

import functions._
import products._
import scala.inline

abstract class Category {

  type Objects

  type C[X <: Objects, Y <: Objects]

  def identity[X <: Objects]: C[X,X]

  def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z]
}

object Category {

  type is[category <: Category] =
    category {
      type Objects = category#Objects
      type C[X <: category#Objects, Y <: category#Objects] = category#C[X,Y]
    }

  @inline final
  def is[category <: Category](c: category): is[category] =
    c.asInstanceOf[is[category]]

  type Opposite[category <: Category] =
    Category {
      type Objects = category#Objects
      type C[X <: Objects, Y <: Objects] = category#C[Y,X]
    }

  @inline final
  def opposite[category <: Category](c: category): Opposite[category] =
    new Category {

      type Objects =
        category#Objects

      type C[X <: Objects, Y <: Objects] =
        category#C[Y,X]

      @inline final
      def identity[X <: Objects]: C[X,X] =
        is(c).identity

      @inline final
      def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
        swap >-> is(c).composition
    }

  type UnitCategory = UnitCategory.type
  case object UnitCategory extends Category {

    type Objects = ∗
    type C[X <: Objects, Y <: Objects] = ∗

    @inline final
    def identity[X <: Objects]: C[X,X] =
      ∗

    @inline final
    def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
      λ { _ => ∗ }
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

        def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
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

  @inline final
  def product[leftCategory <: Category, rightCategory <: Category](l: leftCategory, r: rightCategory): Product[leftCategory,rightCategory] =
    new Category {

      type Objects =
        Tuple {
          type Left   <: leftCategory#Objects
          type Right  <: rightCategory#Objects
        }

      type C[X <: Objects, Y <: Objects] =
        leftCategory#C[X#Left,Y#Left] × rightCategory#C[X#Right, Y#Right]

      @inline final
      def identity[X <: Objects]: C[X,X] =
        is(l).identity and is(r).identity

      @inline final
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

  @inline final
  def hom[Cat <: Category]: Cat -> Hom[Cat] =
    λ { cat: Cat =>

      new Functor {

        type Source = Product[Opposite[Cat], Cat]
        val source = product(opposite(cat), cat)

        type Target = Scala.type
        val target = Scala

        type F[Z <: Source#Objects] = Cat#C[Z#Left, Z#Right]

        def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> (F[X] -> F[Y]) =
          λ { fg =>
            λ { q =>
              is(cat).composition( is(cat).composition(left(fg) and q) and right(fg))
            }
          }
      }
    }
}
