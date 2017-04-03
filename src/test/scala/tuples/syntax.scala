package ohnosequences.stuff.test.tuples

import ohnosequences.stuff._, Function._, products._, Scala._
import ohnosequences.stuff.{Sums => sums}, sums._
import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class TuplesSyntax extends FunSuite {

  val l       = Function { x: String => x.length }
  val toStr   = Function { x: Int => x.toString }
  val isZero  = Function { x: Int => x == 0 }
  val isEmpty = Function { x: String => x.isEmpty }

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

    val x = both(toStr & isZero)
    val y = both(both(toStr & isZero) & isZero)
    val yAgain = both3(toStr & isZero & isZero)

    assert { y(12) === yAgain(12) }
  }
}
