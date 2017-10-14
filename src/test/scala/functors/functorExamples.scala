package ohnosequences.stuff.test.functors

import ohnosequences.stuff._
import ohnosequences.stuff.functions._
import scala.StringContext
import scala.Predef.String
import org.scalatest.FunSuite

case object boh {

  val buh =
    (Functor.identity at Scala) at { λ { x: String => s"hola ${x}!" } }
}

class FunctorsExamples extends FunSuite {

  import boh._

  test("identity functor") {

    assert { (buh at "scalac") === (λ { x: String => s"hola ${x}!" })("scalac") }
  }
}
