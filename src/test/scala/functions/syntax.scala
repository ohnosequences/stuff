package ohnosequences.stuff.test.functions

import ohnosequences.stuff._
import scala.{Boolean, Int}
import scala.Predef.String
import org.scalatest.FunSuite

class FunctionSyntax extends FunSuite {

  test("Declare functions") {

    // use the constructor and pass a closure
    val strLen: String -> Int = { _.length }

    assert { strLen("hola") === 4 }
  }

  test("function composition") {

    // needs a type annotation at the beginning
    // note how the expression associates: ({ ... }) >-> ({ ... })
    val strLenIs2: String -> Boolean = { x: String =>
      x.length
    } >-> { _ == 2 }

    val strLen: String -> Int = _.length

    val is2: Int -> Boolean = { _ == 2 }

    assert {
      { (strLen >-> is2)("hola") } === { strLenIs2("hola") } && {
        (strLen >-> is2)("no")
      } === { strLenIs2("no") }
    }
  }
}
