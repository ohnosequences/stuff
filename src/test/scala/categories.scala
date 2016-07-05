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

case object ListF extends Functor(Scala, Scala) {

  type F[X] = List[X]

  def apply[X,Y](f: X => Y): List[X] => List[Y] = { xs: List[X] => xs map f }
}

case object ListM extends AnyMonad {

  type On = Scala.type
  type Functor = ListF.type
  val functor = ListF

  type μ = mu.type
  val μ = mu
  case object mu extends AnyNaturalTransformation {

    type SourceF = (ListF.type >=> ListF.type)
    lazy val sourceF = ListF >=> ListF
    type TargetF = ListF.type
    lazy val targetF = ListF

    def at[X]: List[List[X]] => List[X] = _.flatten
  }

  type η = unit.type
  val η = unit
  case object unit extends AnyNaturalTransformation {

    type SourceF = AnyFunctor.is[IdentityFunctor[Scala.type]]
    val sourceF = AnyFunctor.is(Scala.Id)

    type TargetF = ListF.type
    val targetF = ListF
    def at[X]: X => List[X] = List(_)
  }
}

case object ScalaProduct extends AnyMonoidalStructure {

  type On = Scala.type
  val on: On = Scala

  type ⊗[X, Y] = (X,Y)
  type ×[X, Y] = X ⊗ Y

  type I = Unit

  // NOTE no bounds needed in this case
  def ⊗[A, B, C, D](f: A => B, g: C => D): (A × C) => (B × D) =
    { case (a,b) => (f(a), g(b)) }

  def ×[A, B, C, D](f: A => B, g: C => D): (A × C) => (B × D) = ⊗(f,g)

  def assoc_right[A, B, C]: (A × B) × C => A × (B × C) =
    { case ((a,b), c) => (a, (b,c)) }

  def assoc_left[A, B, C]: A × (B × C) => (A × B) × C =
    { case (a, (b,c)) => ((a,b), c) }
}

trait AnyMealy {

  type Input
  type State
  type Output

  def apply(i: Input, s: State): (State, Output)
}
trait Mealy[A,U,B] extends AnyMealy with ( (A,U) => (U,B) ) {

  type Input = A
  type State = U
  type Output = B
}

case class mealy[A,U,B](next: (A,U) => (U,B)) extends Mealy[A,U,B] {

  def apply(i: Input, s: State): (State, Output) = next(i,s)
}

trait MealyCat extends AnyCategory {

  type Objects = Any

  type C[A,B] = AnyMealy { type Input = A; type Output = B }

  def id[A] =  mealy[A,Unit,A]{ case (a,u) => (u,a) }

  def compose[X,Y,Z]: (C[Y,Z], C[X,Y]) => C[X,Z] = {

    (m,n) => mealy[X, (n.State, m.State), Z]{
      case (x, (nu,mu)) => {

        val (nu1, y) = n(x, nu)
        val (mu1, z) = m(y, mu)
        ((nu1, mu1), z)
      }
    }
  }
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

  test("monads and kleisli categories") {

    val idMonad = IdentityMonad(Scala.Id)
    val klCat   = KleisliCategory(idMonad)
    val klF     = KleisliFunctor(klCat)
    val uF      = KleisliForget(klCat)

    val f = { x: String => x.length }

    // NOTE why the types are needed here?
    assert { klF[String,Int](f)("hola") === uF[String,Int](f)("hola") }

    val ListKL = KleisliCategory(ListM)

    val ListKLF = KleisliFunctor(ListKL)
    val ListKLU = KleisliForget(ListKL)

    val g = { xs: String => xs.toList }

    println { ListKLU[String,Char](g)(List("hola", "scalac")) }
  }

  test("monoidal structures") {

    import ScalaProduct._
    import AnyMonoidalStructure._

    val f: Scala.C[String, Int]  = { x: String => x.length }
    val g: Scala.C[Int, Boolean] = { x: Int => (x % 2) == 0 }

    val fg = f ⊗ g

    val (a,b) = fg("hola", 12)
  }
}
