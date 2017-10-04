
```scala
package ohnosequences.stuff.test

import ohnosequences.stuff._
import ohnosequences.stuff.products._
import ohnosequences.stuff.functions._

sealed trait AnyMealy {

  type Input
  type State
  type Output

  def apply: (Input × State) -> (State × Output)
}

case class Mealy[I,S,O](val next: (I × S) -> (S × O)) extends AnyMealy {

  type Input  = I
  type State  = S
  type Output = O

  final
  def apply =
    next
}

case object Mealy {

  type between[I,O] =
    AnyMealy {
      type Input  = I
      type Output = O
    }
}

case object Machines extends Category {

  type Objects = Scala.Objects

  type C[X <: Objects, Y <: Objects] = Mealy.between[X,Y]

  final
  def identity[X] =
    Mealy[X,∗,X](swap)

  final
  def composition[X,Y,Z]: C[X,Y] × C[Y,Z] -> C[X,Z] =
    λ { mn =>

      val m = left(mn); val n = right(mn)

      Mealy(
        assoc_left                            >->
        (m.apply × Scala.identity[n.State])   >->
        assoc_right                           >->
        (Scala.identity[m.State] × n.apply)   >->
        assoc_left
      )
    }
}

```




[test/scala/tuples/stdComparison.scala]: tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: functors/functorExamples.scala.md
[test/scala/sums.scala]: sums.scala.md
[test/scala/ScalaCategory.scala]: ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: functions/syntax.scala.md
[test/scala/categories.scala]: categories.scala.md
[main/scala/stuff/monoidalCategories.scala]: ../../main/scala/stuff/monoidalCategories.scala.md
[main/scala/stuff/products.scala]: ../../main/scala/stuff/products.scala.md
[main/scala/stuff/Scala.scala]: ../../main/scala/stuff/Scala.scala.md
[main/scala/stuff/package.scala]: ../../main/scala/stuff/package.scala.md
[main/scala/stuff/sums.scala]: ../../main/scala/stuff/sums.scala.md
[main/scala/stuff/monoids.scala]: ../../main/scala/stuff/monoids.scala.md
[main/scala/stuff/maybe.scala]: ../../main/scala/stuff/maybe.scala.md
[main/scala/stuff/boolean.scala]: ../../main/scala/stuff/boolean.scala.md
[main/scala/stuff/functors.scala]: ../../main/scala/stuff/functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: ../../main/scala/stuff/naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: ../../main/scala/stuff/categories.scala.md
[main/scala/stuff/functions.scala]: ../../main/scala/stuff/functions.scala.md