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
