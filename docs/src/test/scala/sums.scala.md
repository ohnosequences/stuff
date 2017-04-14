
```scala
package ohnosequences.stuff.test

import ohnosequences.stuff._
import ohnosequences.stuff.functions._
import ohnosequences.stuff.sums._
import ohnosequences.stuff.products._

import scala.{ Int }
import scala.Predef.String
import org.scalatest.FunSuite

class Sums extends FunSuite {

  val l       = λ { x: String => x.length }
  val toStr   = λ { x: Int => x.toString }
  val isZero  = λ { x: Int => x == 0 }
  val isEmpty = λ { x: String => x.isEmpty }

  test("either") {

    assert {
      either(isZero and isEmpty)(inL(0))  == isZero(0)     &&
      either(isZero and isEmpty)(inR("")) == isEmpty("")
    }
  }

  test("+") {

    assert {
      (l + toStr)(inR(2))               === inR("2")  &&
      (l + toStr >-> toStr + l)(inR(2)) === inR(1)
    }
  }

  test("any nothing commutative monoid") {

    val l: String + String =
      inL("hola")

    val r: String + String =
      inR("scalac")

    assert {
      any(l) == (sums.swap >-> any[String])(l) &&
      any(r) == (sums.swap >-> any[String])(r)
    }
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
[main/scala/stuff/products.scala]: ../../main/scala/stuff/products.scala.md
[main/scala/stuff/Scala.scala]: ../../main/scala/stuff/Scala.scala.md
[main/scala/stuff/package.scala]: ../../main/scala/stuff/package.scala.md
[main/scala/stuff/sums.scala]: ../../main/scala/stuff/sums.scala.md
[main/scala/stuff/boolean.scala]: ../../main/scala/stuff/boolean.scala.md
[main/scala/stuff/functors.scala]: ../../main/scala/stuff/functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: ../../main/scala/stuff/naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: ../../main/scala/stuff/categories.scala.md
[main/scala/stuff/functions.scala]: ../../main/scala/stuff/functions.scala.md