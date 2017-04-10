package ohnosequences.stuff.test.tuples

import ohnosequences.stuff._, functions._, products._
import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class TuplesSyntax extends FunSuite {

  val l           = λ { x: String => x.length  }
  val toStr       = λ { x: Int => x.toString   }
  val isZero      = λ { x: Int => x == 0       }
  val isPositive  = λ { x: Int => x > -1       }
  val plusOne     = λ { x: Int => x + 1        }
  val isEmpty     = λ { x: String => x.isEmpty }

  test("build tuple values") {

    val buh: (String × Int) -> (Int × String) =
      l × toStr

    val boh: (String -> Int) × (Int -> String) × (String -> Boolean) =
      l & toStr & isEmpty

    assert {
      πL(l and toStr and isEmpty)    === (l and toStr)  &&
      left(l and toStr and isEmpty)  === (l and toStr)  &&
      πR(l and toStr and isEmpty)    === isEmpty        &&
      right(l and toStr and isEmpty) === isEmpty
    }
  }

  test("product universal") {

    val x =
      both(toStr and isZero)

    val y =
      both(both(toStr and isZero) and isZero)

    val yAgain =
      all3(toStr and isZero and isZero)

    assert {
      all3(toStr and isZero and isZero)(1) === ("1" and false and false)  &&
      y(12) === yAgain(12)
    }
  }

  val iterations =
    10000000

  val f = { x: Int => x.toString }
  val g = { x: Int => (x == 0) }
  val h = { x: Int => (x == 0) }
  val f0 = all3(toStr and isZero and isZero)

  test("std evaluation") {

    import scala.Predef._

    for(i <- 1 to iterations) { val zzz = (f(i), g(i), h(i)) }
  }

  test("std inline evaluation") {

    import scala.Predef._

    for(i <- 1 to iterations) {
      val zzz =
        (({ x: Int => x.toString })(i), ({ x: Int => (x == 0) })(i), ({ x: Int => (x == 0) })(i))
    }
  }

  test("allX evaluation") {

    import scala.Predef._

    for(i <- 1 to iterations) { val zzz = f0(i) }
  }
}
