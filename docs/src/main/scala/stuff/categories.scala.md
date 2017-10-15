
```scala
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
  def identity[X <: Objects]: C[X,X]

  /** Morphism composition. */
  def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z]
}

object Category {

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

```




[test/scala/tuples/stdComparison.scala]: ../../../test/scala/tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: ../../../test/scala/tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: ../../../test/scala/functors/functorExamples.scala.md
[test/scala/sums.scala]: ../../../test/scala/sums.scala.md
[test/scala/ScalaCategory.scala]: ../../../test/scala/ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: ../../../test/scala/functions/syntax.scala.md
[test/scala/categories.scala]: ../../../test/scala/categories.scala.md
[main/scala/stuff/monoidalCategories.scala]: monoidalCategories.scala.md
[main/scala/stuff/products.scala]: products.scala.md
[main/scala/stuff/Scala.scala]: Scala.scala.md
[main/scala/stuff/package.scala]: package.scala.md
[main/scala/stuff/sums.scala]: sums.scala.md
[main/scala/stuff/monoids.scala]: monoids.scala.md
[main/scala/stuff/maybe.scala]: maybe.scala.md
[main/scala/stuff/boolean.scala]: boolean.scala.md
[main/scala/stuff/functors.scala]: functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: categories.scala.md
[main/scala/stuff/functions.scala]: functions.scala.md