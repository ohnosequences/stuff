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

    assert { buh("hola" and 2) === (4 and "2") }

    val boh: (String -> Int) × (Int -> String) × (String -> Boolean) =
      l and toStr and isEmpty

    assert { (π_3_3 at boh)("") === true }

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

    assert { x(0) === ("0" and true) }

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

  def checkMe =
    all3(toStr and isZero and isZero)

  val f0 = all3(toStr and isZero and isZero)

  test("std evaluation") {

    import scala.Predef._

    for(i <- 1 to iterations) { (f(i), g(i), h(i)) }
  }

  test("std inline evaluation") {

    import scala.Predef._

    for(i <- 1 to iterations) {
      (({ x: Int => x.toString })(i), ({ x: Int => (x == 0) })(i), ({ x: Int => (x == 0) })(i))
    }
  }

  test("allX evaluation") {

    import scala.Predef._

    for(i <- 1 to iterations) { f0(i) }
  }
}
