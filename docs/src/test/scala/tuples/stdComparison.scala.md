
```scala
package ohnosequences.stuff.test.tuples

import ohnosequences.stuff._, functions._, products._
import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class TuplesVsStd extends FunSuite {

  final val l       = λ { x: String => x.length  }
  final val toStr   = λ { x: Int => x.toString   }
  final val isZero  = λ { x: Int => x == 0       }

  val iterations =
    50000000

  final val f = { x: Int => x.toString }
  final val g = { x: Int => (x == 0) }
  final val h = { x: Int => (x == 0) }

  final val tupleSyntax =
    all3(toStr and isZero and isZero)

  final val scalaFns =
    λ { i: Int => (f(i), g(i), h(i)) }

  test("tuple syntax evaluation") {

    var i = 1
    var z: String × Boolean × Boolean = null

    while(i <= iterations) {
      z = tupleSyntax(i)
      i = i + 1
    }
  }

  test("std evaluation") {

    var i = 1
    var z: (String, Boolean, Boolean) = null

    while(i <= iterations) {
      z = scalaFns(i)
      i = i + 1
    }
  }

  test("std inline evaluation") {

    var i = 1
    var z: (String, Boolean, Boolean) = null

    while(i <= iterations) {
      z = (({ x: Int => x.toString })(i), ({ x: Int => (x == 0) })(i), ({ x: Int => (x == 0) })(i))
      i = i + 1
    }
  }
}

```




[test/scala/tuples/stdComparison.scala]: stdComparison.scala.md
[test/scala/tuples/syntax.scala]: syntax.scala.md
[test/scala/functors/functorExamples.scala]: ../functors/functorExamples.scala.md
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