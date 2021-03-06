package ohnosequences.stuff.test

import ohnosequences.stuff._

import scala.{Int}
import scala.Predef.String
import org.scalatest.FunSuite

class ScalaCategoryTests extends FunSuite {

  val l: String -> Int = { x: String =>
    x.length
  }
  val toStr: Int -> String = { x: Int =>
    x.toString
  }
  // val idInt : Int -> Int = Scala.identity

  val uh: (Int -> String) -> (String -> Int) =
    Category
      .homFunctor(Scala)
      .at[Int × String, String × Int](l and l) // no good inference here
  // Category.hom(Scala)(l and l)

  test("Hom functor") {

    assert { uh(toStr)("hola") === 1 }
  }

  test("composition and identity") {

    assert {
      Scala.composition(Scala.identity[String] and Scala.identity[String])(
        "hola") === "hola"
    }

    assert {
      Scala.composition(Scala.identity[Int] and toStr)(234243) === toStr(234243)
    }
  }

  test("syntax") {

    Product(ohnosequences.stuff.tuples) ⊢ {

      val z0: String >=> String =
        id[String] >=> id[String]

      val z1: String >=> String =
        id[String]

      val u: (String × Int) >=> (Int × String) =
        l × toStr

      val _2and2: Int × String =
        u("ab" and 2)

      assert { z0("hola") === "hola" }
      assert { z1("hola") === "hola" }
      assert { left(_2and2) === 2 }
      assert { right(_2and2) === "2" }
    }
  }

  test("Distributive category") {

    DistributiveCategory(ScalaDist) ⊢ {

      val u0 =
        l × (l + toStr)

      val u1 =
        u0 >=> expand >=> pack

      val x: String × (String + Int) =
        "hola" and inRight(2)

      assert { (u0 at x) == (u1 at x) }
    }
  }
}
