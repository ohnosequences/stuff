package ohnosequences.stuff.test

import ohnosequences.stuff._, AnyFunction._, Sums._, Product._
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
}
