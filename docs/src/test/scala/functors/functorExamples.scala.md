
```scala
package ohnosequences.stuff.test.functors

import ohnosequences.stuff._
import ohnosequences.stuff.functions._
import scala.StringContext
import scala.Predef.String
import org.scalatest.FunSuite

case object boh {

  val buh =
    (functors.identity at Scala) at { λ { x: String => s"hola ${x}!" } }
}

class FunctorsExamples extends FunSuite {

  import boh._

  test("identity functor") {

    assert { (buh at "scalac") === (λ { x: String => s"hola ${x}!" })("scalac") }
  }
}

```




[test/scala/tuples/stdComparison.scala]: ../tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: ../tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: functorExamples.scala.md
[test/scala/sums.scala]: ../sums.scala.md
[test/scala/ScalaCategory.scala]: ../ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: ../functions/syntax.scala.md
[test/scala/categories.scala]: ../categories.scala.md
[main/scala/stuff/products.scala]: ../../../main/scala/stuff/products.scala.md
[main/scala/stuff/Scala.scala]: ../../../main/scala/stuff/Scala.scala.md
[main/scala/stuff/package.scala]: ../../../main/scala/stuff/package.scala.md
[main/scala/stuff/sums.scala]: ../../../main/scala/stuff/sums.scala.md
[main/scala/stuff/boolean.scala]: ../../../main/scala/stuff/boolean.scala.md
[main/scala/stuff/functors.scala]: ../../../main/scala/stuff/functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: ../../../main/scala/stuff/naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: ../../../main/scala/stuff/categories.scala.md
[main/scala/stuff/functions.scala]: ../../../main/scala/stuff/functions.scala.md