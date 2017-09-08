package ohnosequences.stuff.test

import ohnosequences.stuff._
import ohnosequences.stuff.functions._
import ohnosequences.stuff.products._

import scala.{ Int }
import scala.Predef.String
import org.scalatest.FunSuite

class ScalaCategoryTests extends FunSuite {

  val l     : String -> Int = λ { x: String => x.length }
  val toStr : Int -> String = λ { x: Int => x.toString  }
  // val idInt : Int -> Int = Scala.identity

  val uh: (Int -> String) -> (String -> Int) =
    Category.hom(Scala).at[Int × String, String × Int](l and l) // no good inference here
    // Category.hom(Scala)(l and l)

  test("Hom functor") {

    assert {  ( (uh at toStr) at "hola" ) === 1 }
  }

  test("composition and identity") {

    assert { Scala.composition( Scala.identity[String] and Scala.identity[String] )("hola") === "hola"  }

    assert { Scala.composition( Scala.identity[Int] and toStr )(234243) === toStr(234243)  }
  }


  test("syntax") {

    def boh[X] =
      Scala ⊢ {
        
        id[X] >=> id[X]
      }

    assert { (boh[String] at "hola") === "hola" }

    def zz =
      products ⊢ { id[String] }

    assert { (zz at "hola") === "hola" }

    def u =
      products ⊢ { l ⊗ toStr }

    val _2and2 = (u at ("ab" and 2))

    assert { left(_2and2) === 2 && right(_2and2) === "2" }
  }
}
