package ohnosequences.stuff.test.functors

import ohnosequences.stuff._
import scala.StringContext
import scala.Predef.String
import org.scalatest.FunSuite

case object boh {

  val Id =
    Functor.identity at Scala

  val buh =
    Id at {
      λ { x: String =>
        s"hola ${x}!"
      }
    }

  val IdTwice =
    Id >-> Id

  val IdTwiceAgain =
    Id ∘ Id
}

class FunctorsExamples extends FunSuite {

  import boh._

  test("identity functor") {

    assert {
      (buh at "scalac") === (λ { x: String =>
        s"hola ${x}!"
      })("scalac")
    }
  }
}
