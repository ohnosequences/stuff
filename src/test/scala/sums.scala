package ohnosequences.stuff.test

import ohnosequences.stuff._
import ohnosequences.stuff.functions._
import ohnosequences.stuff.sums._
import ohnosequences.stuff.products._

import scala.{Int}
import scala.Predef.String
import org.scalatest.FunSuite

class Sums extends FunSuite {

  val l = λ { x: String =>
    x.length
  }
  val toStr = λ { x: Int =>
    x.toString
  }
  val isZero = λ { x: Int =>
    x == 0
  }
  val isEmpty = λ { x: String =>
    x.isEmpty
  }

  test("either") {

    assert {
      either(isZero and isEmpty)(inL(0)) == isZero(0) &&
      either(isZero and isEmpty)(inR("")) == isEmpty("")
    }
  }

  test("+") {

    assert {
      (l + toStr)(inR(2)) === inR("2") &&
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
