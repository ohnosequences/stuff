package ohnosequences.stuff.test

import ohnosequences.stuff._, Function._, Product._, Scala._
import ohnosequences.stuff.{Sums => sums}, sums._
import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class Sums extends FunSuite {

  val l       = Function { x: String => x.length }
  val toStr   = Function { x: Int => x.toString }
  val isZero  = Function { x: Int => x == 0 }
  val isEmpty = Function { x: String => x.isEmpty }

  test("either") {


    assert {
      either(isZero x isEmpty)(inL(0)) == isZero(0)     &&
      either(isZero x isEmpty)(inR("")) == isEmpty("")
    }
  }

  test("+") {

    assert {
      (l + toStr)(inR(2))                   === inR("2")  &&
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
