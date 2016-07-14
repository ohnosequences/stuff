package ohnosequences.stuff.test

import org.scalatest.FunSuite
import ohnosequences.stuff._

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

  def id[A]: C[A,A] =
    mealy[A,Unit,A]{ case (a,u) => (u,a) }

  def compose[X,Y,Z]: (C[Y,Z], C[X,Y]) => C[X,Z] = {

    (m,n) => mealy[X, (n.State, m.State), Z] {

      case (x, (nu,mu)) => {

        val (nu1, y) = n(x, nu)
        val (mu1, z) = m(y, mu)
        ((nu1, mu1), z)
      }
    }
  }
}

case object Mealy extends MealyCat

case object Scala extends AnyCategory {

  type Objects = Any
  type C[A,B] = A => B

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] = (g,f) => f andThen g
  def id[X <: Objects]: C[X,X] = { x: X => x }
}


case object ScalaSums extends AnyCoproducts {

  type On     = Scala.type
  val on: On  = Scala

  type ⊗[X, Y] = X Either Y
  type I = Nothing

  def left  [A <: On#Objects, B <: On#Objects]: A => A + B =
    a => Left(a)

  def right [A <: On#Objects, B <: On#Objects]: B => A + B =
    b => Right(b)

  def nothing [A <: On#Objects]: Nothing => A =
    Predef.identity[Nothing]

  def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]: (A => X, B => X) => (A + B => X) =
    (f,g) => { ab => ab.fold(f,g) }

  def assoc_right[A, B, C]: (A + B) + C => A + (B + C) =
    {
      ab_c => ab_c match {

        case Left(ab) => ab match {

          case Left(a)  => Left(a)
          case Right(b) => Right(Left(b))
        }

        case Right(c) => Right(Right(c))
      }
    }

  def assoc_left[A, B, C]: A + (B + C) => (A + B) + C =
    {
      a_bc => a_bc match {

        case Left(a) => Left(Left(a))

        case Right(bc) => bc match {

          case Left(b)  => Left(Right(b))
          case Right(c) => Right(c)
        }
      }
    }
}


class MealyTests extends FunSuite {

  test("identity monad") {

    val idMonad     = IdentityMonad(Scala)
    val idMonadKl   = idMonad kleisliCategory
    val kleisliSums = idMonadKl coproductsFrom ScalaSums
  }
}
