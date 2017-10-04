
```scala
package ohnosequences.stuff

import functions._

/**
  =The Scala category=

  This object is the [[Category]] corresponding to all types as objects and functions as morphisms.
*/
object Scala extends Category {

  /** Every type is a subtype of [[http://www.scala-lang.org/api/2.12.3/scala/Any.html Any]]. */
  type Objects =
    scala.Any

  /** Morphisms between `X` and `Y` are functions between them. */
  type C[X,Y] =
    X -> Y

  /** The identity function `x: X => x` */
  @inline final
  def identity[X <: Objects]: C[X,X] =
    functions.identity[X]

  /** Function composition. */  
  @inline final
  def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
    λ { fg => fg.left >-> fg.right }
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