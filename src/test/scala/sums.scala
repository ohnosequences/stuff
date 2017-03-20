package ohnosequences.stuff.test

import ohnosequences.stuff._, AnyFunction._, Product._, Scala._
import ohnosequences.stuff.{Sums => sums}, sums._
import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class Sums extends FunSuite {

  test("either") {

    val l       = function { x: String => x.length }
    val toStr   = function { x: Int => x.toString }
    val isZero  = function { x: Int => x == 0 }
    val isEmpty = function { x: String => x.isEmpty }

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
      any(l) == (sums.swap >=> any[String])(l) &&
      any(r) == (sums.swap >=> any[String])(r)
    }
  }
}
