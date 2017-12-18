package ohnosequences.stuff.test

import ohnosequences.stuff._

import scala.Int
import scala.Predef.String
import org.scalatest.FunSuite

class Sums extends FunSuite {

  val l = { x: String =>
    x.length
  }
  val toStr = { x: Int =>
    x.toString
  }
  val isZero = { x: Int =>
    x == 0
  }
  val isEmpty = { x: String =>
    x.isEmpty
  }

  test("either") {

    Coproduct(sums) ⊢ {

      assert {

        ((isZero | isEmpty) at left(0)) == (isZero at 0) &&
        ((isZero | isEmpty) at right("")) == isEmpty("")
      }
    }
  }

  test("+") {

    Coproduct(sums) ⊢ {

      assert {
        ((l + toStr) at right(2)) == right("2") && {
          (l + toStr) >=> (toStr + l) at right(2)
        } == right(1)
      }
    }
  }

  test("any nothing commutative monoid") {

    Coproduct(sums) ⊢ {

      val l: String + String =
        left at "hola"

      val r: String + String =
        right at "scalac"

      assert {
        any(l) == { swap >=> any[String] at l } &&
        any(r) == { swap >=> any[String] at r }
      }
    }
  }
}
