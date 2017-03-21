package ohnosequences.stuff.test

import ohnosequences.stuff._, Function._, Product._, Scala._
import ohnosequences.stuff.{Sums => sums}, sums._
import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class Sums extends FunSuite {

  test("either") {

    val l       = Function { x: String => x.length }
    val toStr   = Function { x: Int => x.toString }
    val isZero  = Function { x: Int => x == 0 }
    val isEmpty = Function { x: String => x.isEmpty }

    assert {
      either(isZero × isEmpty)(inL(0)) == isZero(0)     &&
      either(isZero × isEmpty)(inR("")) == isEmpty("")
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
