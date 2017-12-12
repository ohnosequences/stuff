package ohnosequences.stuff.test.functions

import ohnosequences.stuff._
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
}
