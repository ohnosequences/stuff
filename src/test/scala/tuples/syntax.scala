package ohnosequences.stuff.test.tuples

import ohnosequences.stuff._, functions._, products._
import scala.{Boolean, Int}
import scala.Predef.String
import org.scalatest.FunSuite

class TuplesSyntax extends FunSuite {

  final val l = λ { x: String =>
    x.length
  }
  final val toStr = λ { x: Int =>
    x.toString
  }
  final val isZero = λ { x: Int =>
    x == 0
  }
  final val isPositive = λ { x: Int =>
    x > -1
  }
  final val plusOne = λ { x: Int =>
    x + 1
  }
  final val isEmpty = λ { x: String =>
    x.isEmpty
  }

  // TODO fix tuple equality
  ignore("build tuple values") {

    val buh: (String × Int) -> (Int × String) =
      l × toStr

    assert { buh("hola" and 2) === (4 and "2") }

    val boh: (String -> Int) × (Int -> String) × (String -> Boolean) =
      l and toStr and isEmpty

    assert { (π_3_3 at boh)("") === true }

    assert {
      πL(l and toStr and isEmpty) === (l and toStr) &&
      left(l and toStr and isEmpty) === (l and toStr) &&
      πR(l and toStr and isEmpty) === isEmpty &&
      right(l and toStr and isEmpty) === isEmpty
    }
  }

  // TODO fix tuple equality
  ignore("product universal") {

    val x =
      both(toStr and isZero)

    assert { x(0) === ("0" and true) }

    val y =
      both(both(toStr and isZero) and isZero)

    val yAgain =
      all3(toStr and isZero and isZero)

    assert {
      all3(toStr and isZero and isZero)(1) === ("1" and false and false) &&
      y(12) === yAgain(12)
    }
  }
}
