package ohnosequences.stuff.test.functions

import ohnosequences.stuff._, functions._, products._
import scala.{ Int, Boolean }
import scala.Predef.String
import org.scalatest.FunSuite

class FunctionSyntax extends FunSuite {

  test("Declare functions") {

    // use the λ constructor and pass a closure
    val strLen: String -> Int =
      λ { _.length }

    // automatic conversions:
    val asFunction: Int -> Boolean =
      { x: Int => x == 2 }
  }

  test("function composition") {

    // needs a type annotation at the beginning
    // note how the expression associates: (λ { ... }) >-> (λ { ... })
    val strLenIs2 =
      λ { x: String => x.length } >-> λ { _ == 2 }

    val strLen: String -> Int =
      λ { _.length }

    val is2 =
      λ { (_: Int) == 2 }

    assert {
      { strLen >-> is2 at "hola" } === { strLenIs2("hola")  } &&
        { strLen >-> is2 at "no" } === { strLenIs2("no") }
    }
  }

  def η[A,X,Y]: ((A × X) -> Y) -> (A -> (X -> Y)) =
    λ { f => λ { a => λ { x => f at (a & x) } } }

  test("η conversion") {

    val strLen: String -> Int =
      λ { _.length }

    val sum: (Int × Int) -> Int =
      λ { xy => left(xy) + right(xy) }

    val f: (Int × String) -> Int =
      identity × strLen >-> sum

    val sumCurried =
      η at sum

    val plus2 =
      η(sum)(2)

    assert { (plus2 at 3) === sum (2 and 3) }
  }

  test("ccc") {

    val strLen: String -> Int =
      λ { _.length }

    assert { ev("hola" and strLen) === (strLen at "hola")  }
  }
}
