package ohnosequences.stuff.test

import ohnosequences.stuff._
import scala.{Boolean, Int}
import scala.Predef.String
import org.scalatest.FunSuite

class TuplesVsStd extends FunSuite {

  val l = { x: String =>
    x.length
  }
  val toStr = { x: Int =>
    x.toString
  }
  val isZero = { x: Int =>
    x == 0
  }

  val iterations =
    500000

  val f = { x: Int =>
    x.toString
  }
  val g = { x: Int =>
    (x == 0)
  }
  val h = { x: Int =>
    (x == 0)
  }

  val tupleSyntax =
    Product(tuples) ⊢ { toStr ^ isZero ^ isZero }

  val scalaFns = { i: Int =>
    (f(i), g(i), h(i))
  }

  test("std inline evaluation") {

    var i                             = 1
    var z: (String, Boolean, Boolean) = null

    while (i <= iterations) {
      z = (({ x: Int =>
        x.toString
      })(i), ({ x: Int =>
        (x == 0)
      })(i), ({ x: Int =>
        (x == 0)
      })(i))
      i = i + 1
    }
  }

  test("tuple syntax evaluation") {

    var i                             = 1
    var z: String × Boolean × Boolean = null

    while (i <= iterations) {
      z = tupleSyntax(i)
      i = i + 1
    }
  }

  test("std evaluation") {

    var i                             = 1
    var z: (String, Boolean, Boolean) = null

    while (i <= iterations) {
      z = scalaFns(i)
      i = i + 1
    }
  }
}
