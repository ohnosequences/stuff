package ohnosequences.stuff.test

import org.scalatest.FunSuite

import ohnosequences.stuff._

case object Scala extends AnyCategory {

  type Objects  = Any
  type C[X,Y]   = X => Y

  final def id[X <: Objects]: C[X,X] =
    { x: X => x }

  final def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] =
    (g, f) => f andThen g

  val Id = IdentityFunctor(Scala)
}



class ScalaCategoryTest extends FunSuite {

  import AnyCategory._
  import Scala._

  test("Syntax for Scala category") {

    val l = AnyCategory.MorphismsSyntax(Scala.id[String]) >=> { x: String => x.length }

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

    assert { Scala.Id.id[String]("") === Scala.id[String]("") }

    val zz = Scala.Id.id >=> Scala.Id.id

    assert { (Scala.Id.id >=> Scala.Id.id >=> Scala.Id.id >=> Scala.Id.id)[Boolean](true) === Scala.id[Boolean](true) }
  }
}
