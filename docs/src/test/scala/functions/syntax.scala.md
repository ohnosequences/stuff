
```scala
package ohnosequences.stuff.test.functions

import ohnosequences.stuff._, products._, functions._
import scala.{ Int }
import scala.Predef.String
import org.scalatest.FunSuite

class FunctionSyntax extends FunSuite {

  test("Declare functions") {

    // use the λ constructor and pass a closure
    val strLen: String -> Int =
      λ { _.length }

    assert { (strLen at "hola") === 4 }
  }

  test("function composition") {

    // needs a type annotation at the beginning
    // note how the expression associates: (λ { ... }) >-> (λ { ... })
    val strLenIs2 =
      λ { x: String => x.length } >-> λ { _ == 2 }

    val strLen: String -> Int =
      λ { _.length }

    val is2 =
      λ { (_: Int) == 2 }

    assert {
      { strLen >-> is2 at "hola" } === { strLenIs2("hola")  } &&
        { strLen >-> is2 at "no" } === { strLenIs2("no") }
    }
  }

  // def η[A,X,Y]: ((A × X) -> Y) -> (A -> (X -> Y)) =
  //   λ { f => λ { a => λ { x => f at (a and x) } } }

  test("η conversion") {

    val strLen: String -> Int =
      λ { _.length }

    val sum: (Int × Int) -> Int =
      λ { xy => left(xy) + right(xy) }

    val f: (Int × String) -> Int =
      identity × strLen >-> sum

    val sumCurried =
      η at sum

    val plus2 =
      η(sum)(2)

    assert { (f(2 and "four")) === 6 }

    assert { (sumCurried at 2)(4) === 6 }

    assert { (plus2 at 3) === sum (2 and 3) }
  }

  test("ccc") {

    val strLen: String -> Int =
      λ { _.length }

    assert { ev("hola" and strLen) === (strLen at "hola")  }
  }
}

```




[test/scala/tuples/stdComparison.scala]: ../tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: ../tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: ../functors/functorExamples.scala.md
[test/scala/sums.scala]: ../sums.scala.md
[test/scala/ScalaCategory.scala]: ../ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: syntax.scala.md
[test/scala/categories.scala]: ../categories.scala.md
[main/scala/stuff/monoidalCategories.scala]: ../../../main/scala/stuff/monoidalCategories.scala.md
[main/scala/stuff/products.scala]: ../../../main/scala/stuff/products.scala.md
[main/scala/stuff/Scala.scala]: ../../../main/scala/stuff/Scala.scala.md
[main/scala/stuff/package.scala]: ../../../main/scala/stuff/package.scala.md
[main/scala/stuff/sums.scala]: ../../../main/scala/stuff/sums.scala.md
[main/scala/stuff/monoids.scala]: ../../../main/scala/stuff/monoids.scala.md
[main/scala/stuff/maybe.scala]: ../../../main/scala/stuff/maybe.scala.md
[main/scala/stuff/boolean.scala]: ../../../main/scala/stuff/boolean.scala.md
[main/scala/stuff/functors.scala]: ../../../main/scala/stuff/functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: ../../../main/scala/stuff/naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: ../../../main/scala/stuff/categories.scala.md
[main/scala/stuff/functions.scala]: ../../../main/scala/stuff/functions.scala.md