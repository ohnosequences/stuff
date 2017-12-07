package ohnosequences.stuff.test

import ohnosequences.stuff._
import scala.Int
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

  test("build tuple values") {

    Product(tuples) ⊢ {

      val buh: (String × Int) -> (Int × String) =
        l × toStr

      assert { buh("hola" and 2) === (4 and "2") }

      assert {
        left(l and toStr and isEmpty) === (l and toStr) &&
        right(l and toStr and isEmpty) === isEmpty
      }
    }
  }

  test("product universal") {

    Product(tuples) ⊢ {

      val x =
        toStr ^ isZero

      assert { x(0) === ("0" and true) }

      val y =
        toStr ^ isZero ^ isZero

      assert { y(1) === ("1" and false and false) }
    }
  }
}
