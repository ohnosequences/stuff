package ohnosequences.stuff.test.functions

import ohnosequences.stuff._, functions._
import scala.{Int}
import scala.Predef.String
import org.scalatest.FunSuite

class FunctionSyntax extends FunSuite {

  test("Declare functions") {

    // use the λ constructor and pass a closure
    val strLen: String -> Int =
      λ { _.length }

    assert { (strLen at "hola") === 4 }
  }

  test("function composition") {

    // needs a type annotation at the beginning
    // note how the expression associates: (λ { ... }) >-> (λ { ... })
    val strLenIs2 =
      λ { x: String =>
        x.length
      } >-> λ { _ == 2 }

    val strLen: String -> Int =
      λ { _.length }

    val is2 =
      λ { (_: Int) == 2 }

    assert {
      { strLen >-> is2 at "hola" } === { strLenIs2("hola") } && {
        strLen >-> is2 at "no"
      } === { strLenIs2("no") }
    }
  }

  test("η conversion") {

    val strLen: String -> Int =
      λ { _.length }

    val sum: (Int × Int) -> Int =
      λ { xy =>
        xy.left + xy.right
      }

    val f: (Int × String) -> Int =
      identity × strLen >-> sum

    val sumCurried =
      η at sum

    val plus2 =
      η(sum)(2)

    assert { (f(2 and "four")) === 6 }

    assert { (sumCurried at 2)(4) === 6 }

    assert { (plus2 at 3) === sum(2 and 3) }
  }
}
