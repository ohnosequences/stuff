package ohnosequences.stuff.test.tuples

import ohnosequences.stuff._, Function._, products._
import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class TuplesSyntax extends FunSuite {

  val l           = Function { x: String => x.length }
  val toStr       = Function { x: Int => x.toString }
  val isZero      = Function { x: Int => x == 0 }
  val isPositive  = Function { x: Int => x > -1 }
  val plusOne     = Function { x: Int => x + 1 }
  val isEmpty     = Function { x: String => x.isEmpty }

  test("build tuple values") {

    val buh: (String × Int) -> (Int × String) =
      l × toStr

    val boh: (String -> Int) × (Int -> String) × (String -> Boolean) =
      l & toStr & isEmpty

    assert {
      πL(l & toStr & isEmpty)    === (l & toStr)  &&
      left(l & toStr & isEmpty)  === (l & toStr)  &&
      πR(l & toStr & isEmpty)    === isEmpty      &&
      right(l & toStr & isEmpty) === isEmpty
    }
  }

  test("product universal") {

    val x =
      both(toStr & isZero)

    val y =
      both(both(toStr & isZero) & isZero)

    val yAgain =
      all3(toStr & isZero & isZero)

    assert {
      all3(toStr & isZero & isZero)(1) === ("1" & false & false)    &&
      // all3(isPositive & isZero & plusOne)(1) === (true & false & 2) && // awful; & is a method on Boolean
      y(12) === yAgain(12)
    }
  }
}
