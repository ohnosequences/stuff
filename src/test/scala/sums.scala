package ohnosequences.stuff.test

import ohnosequences.stuff._, Function._, sums._, products._, Scala._

import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class Sums extends FunSuite {

  val l       = λ { x: String => x.length }
  val toStr   = λ { x: Int => x.toString }
  val isZero  = λ { x: Int => x == 0 }
  val isEmpty = λ { x: String => x.isEmpty }

  test("either") {


    assert {
      either(isZero & isEmpty)(inL(0))  == isZero(0)     &&
      either(isZero & isEmpty)(inR("")) == isEmpty("")
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
