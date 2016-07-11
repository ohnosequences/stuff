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

case object MealyCat extends AnyCategory {

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

case object Scala extends AnyCategory {

  // this is needed because of [SI-8709](https://issues.scala-lang.org/browse/SI-8709)
  final case class Function[A,B](val raw: A => B) extends AnyVal {}
  implicit def toFunction[A,B](f: A => B): Function[A,B] = Function(f)

  type Objects = Any
  type C[X,Y] = Function[X,Y]

  def id[X] = { x: X => x }

  def compose[X,Y,Z]: (C[Y,Z], C[X,Y]) => C[X,Z] = (g,f) => f.raw andThen g.raw
}

class MealyTests extends FunSuite {

  test("local syntax") {

    val m = mealy( (x: String, u: Int) => if(x.isEmpty) (u, x) else (u + 1, x drop 1) ) :<: MealyCat
    val n = mealy( (x: String, b: Boolean) => if(b) (false, x) else (true, x drop 1)  ) :<: MealyCat

    val mm = imply(MealyCat) { m >=> n }

    val f = { x: String => x.length } :<: Scala
    val g = { n: Int => if(n % 2 == 0) true else false } :<: Scala

    val fg = f >=> g

    val kleisliId = IdentityMonad(Scala).kleisliCategory
    val fg2: kleisliId.C[String,Boolean] = imply(kleisliId) { f >=> g }

    val fg0 = imply(Scala) {

      { x: String => x.length } :<: Scala >=> { n: Int => if(n % 2 == 0) true else false }
    }
    val fg0_again = imply(Scala) {

      ({ x: String => x.length } :<: Scala) >=> { n: Int => if(n % 2 == 0) true else false }
    }
  }
}
