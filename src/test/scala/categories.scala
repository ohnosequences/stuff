package ohnosequences.stuff.test

import org.scalatest.FunSuite

import ohnosequences.stuff._

case object Scala extends AnyCategory {

  type Objects    = Any
  type Morphisms  = AnyFunction

  type C[X,Y] = Function[X,Y]

  final def id[X <: Objects]: C[X,X] =
    { x: X => x }

  final def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] =
    (g, f) => f andThen g

  trait AnyFunction extends Any with AnyMorphism {

    type Bound = Any

    def f: Source => Target

    final def apply(a: Source): Target = f(a)
  }

  case object AnyFunction {

    type is[F <: AnyFunction] = F with AnyFunction {

      type Source = F#Source
      type Target = F#Target
    }
  }
  case class Function[A,B](val f: A => B) extends AnyVal with AnyFunction {

    type Source = A
    type Target = B
  }
  // wrap, unwrap
  implicit def asFunction[A0 <: Objects, B0 <: Objects](f: A0 => B0): Function[A0,B0] = new Function(f)
  implicit def asFunction1Again[F <: Morphisms](ff: AnyMorphism.is[F]): F#Source => F#Target = ff.f

  case object Id extends IdentityFunctor(Scala)
  // case object IdNat extends IdentityNaturalTransformation(Id)
}



class ScalaCategoryTest extends FunSuite {

  test("Syntax for Scala category") {

    def in[
      Cat <: AnyCategory,
      X <: Cat#Objects,
      Y <: Cat#Objects
    ]
    (cat: AnyCategory.is[Cat])(expr: => Cat#C[X,Y]): Cat#C[X,Y] = expr

    val zz = in(Scala) { { x: String => x.length } } ∘ Scala.id[String]

    val l = ({ x: String => x.length }: Scala.C[String,Int]) ∘ Scala.id[String]
    // NOTE won't work, I don't know why. You need o explicitly ascribe at the beginning.
    // val l2 = { x: String => x.length } ∘ Scala.id[String]

    val f = { x: Int => x.toString }

    val zzz = l >=> f

    val www = (f: Scala.C[Int,String]) >=> Scala.id[String] >=> { x: String => x.length }
  }

  test("Functors on Scala") {

    val f: Scala.C[Int,String] = { x: Int => x.toString }

    assert { Scala.Id(f) === f }

    val IdTwice = Scala.Id >=> Scala.Id

    assert { IdTwice(f) === f }

  }

  test("Natural transformations on Scala") {

    val idNat = new IdentityNaturalTransformation(Scala.Id)
    assert { idNat[String]("") === Scala.id[String]("") }
  }
}
