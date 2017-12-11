package ohnosequences.stuff.test.functors

import ohnosequences.stuff._
import scala.StringContext
import scala.Predef.String
import org.scalatest.FunSuite

case object boh {

  val Id: Functor.is[Functor.Identity[Scala]] =
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

  // 2-cells
  import scala.Predef.implicitly
  import NaturalTransformation._
  val idNat: Identity[Functor.Identity[Scala]] =
    identity[Functor.Identity[Scala]](Id)

  class TC {

    type F[X]
  }

  type Is[tc <: TC] = tc { type F[X] = tc#F[X] }

  object IDTC extends TC { type F[X] = X }

  implicitly[IDTC.type <:< Is[IDTC.type]]

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
