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

  val idMonad =
    Monad idMonad Scala

  val kl =
    KleisliCategory of idMonad

  val z =
    Category(kl) ⊢ {
      val f: String >=> String =
        buh

      f >=> f
    }
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
