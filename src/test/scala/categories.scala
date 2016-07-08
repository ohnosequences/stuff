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

  def id[A] =
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
