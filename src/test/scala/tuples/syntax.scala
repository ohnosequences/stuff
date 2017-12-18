package ohnosequences.stuff.test

import ohnosequences.stuff._
import scala.{Boolean, Int}
import scala.Predef.String
import org.scalatest.FunSuite

class TuplesSyntax extends FunSuite {

  val l: String -> Int = { x: String =>
    x.length
  }
  val toStr: Int -> String = { x: Int =>
    x.toString
  }
  val isZero: Int -> Boolean = { x: Int =>
    x == 0
  }
  val isPositive = { x: Int =>
    x > -1
  }
  val plusOne = { x: Int =>
    x + 1
  }
  val isEmpty = { x: String =>
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
