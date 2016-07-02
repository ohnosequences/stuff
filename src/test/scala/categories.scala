package ohnosequences.stuff.test

import org.scalatest.FunSuite

import ohnosequences.stuff._

case object Scala extends AnyCategory {

  implicit def whyNot[A0,B0](f: A0 => B0): C[A0,B0] = f

  type Objects    = Any
  type C[X,Y] = X => Y

  final def id[X <: Objects]: C[X,X] =
    { x: X => x }

  final def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] =
    (g, f) => f andThen g

  val Id = new IdentityFunctor(Scala)
  // case object IdNat extends IdentityNaturalTransformation(Id)
}



class ScalaCategoryTest extends FunSuite {

  import AnyCategory._

  test("Syntax for Scala category") {

    val l = AnyCategory.MorphismsSyntax(Scala.id[String]) >=> { x: String => x.length }
    // NOTE won't work, I don't know why. You need o explicitly ascribe at the beginning.
    // val l2 = { x: String => x.length } ∘ Scala.id[String]

    val f = { x: Int => x.toString }

    val www = (f: Scala.C[Int,String]) >=> Scala.id[String] >=> { x: String => x.length }
  }

  test("Functors on Scala") {

    val f: Scala.C[Int,String] = { x: Int => x.toString }

    assert { Scala.Id(f) === f }

    val IdTwice = new FunctorComposition(Scala.Id, Scala.Id)

    assert { IdTwice(f) === f }

  }

  test("Natural transformations on Scala") {

    val idNat = IdentityNaturalTransformation(Scala.Id)
    assert { Scala.Id.id[String]("") === Scala.id[String]("") }
  }
}
