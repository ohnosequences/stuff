
```scala
package ohnosequences.stuff

import products._, sums._, functions._

case object booleans {

  import scala.Predef.???

  type Ω = (∗ + ∗)

  val ⊤ : Ω = inR(∗)
  val ⊥ : Ω = inL(∗)

  val fromBoolean: scala.Boolean -> Ω =
    λ { b: scala.Boolean => if(b) ⊤ else ⊥  }

  val toBoolean: Ω -> scala.Boolean =
    either { point(true) and point(false) }

  def eq[A]: A × A -> Ω =
    λ { as: A × A => left(as) == right(as) } >-> fromBoolean

  val ∧ : Ω × Ω -> Ω =
    ???

  val ∨ : Ω × Ω -> Ω =
    ???

  // better a curried version?
  final
  def If[X,Y]: (X -> Ω) -> (((∗ -> Y) × (∗ -> Y)) -> (X -> Y)) =
    λ { p => λ { tf => p >-> either(tf) } }

  import scala.Int

  val isEven: Int -> Ω =
    λ { x => eq(x % 2 and 0) }

  val msg =
    point("Is Even!")

  val other =
    point("Is odd!")

  val tellMe =
    If(isEven)(msg and other)
}

```




[test/scala/tuples/stdComparison.scala]: ../../../test/scala/tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: ../../../test/scala/tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: ../../../test/scala/functors/functorExamples.scala.md
[test/scala/sums.scala]: ../../../test/scala/sums.scala.md
[test/scala/ScalaCategory.scala]: ../../../test/scala/ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: ../../../test/scala/functions/syntax.scala.md
[test/scala/categories.scala]: ../../../test/scala/categories.scala.md
[main/scala/stuff/products.scala]: products.scala.md
[main/scala/stuff/Scala.scala]: Scala.scala.md
[main/scala/stuff/package.scala]: package.scala.md
[main/scala/stuff/sums.scala]: sums.scala.md
[main/scala/stuff/boolean.scala]: boolean.scala.md
[main/scala/stuff/functors.scala]: functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: categories.scala.md
[main/scala/stuff/functions.scala]: functions.scala.md