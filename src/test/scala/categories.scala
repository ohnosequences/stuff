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
  final case class Function[A,B](val raw: A => B) extends AnyVal { def apply(x: A): B = raw(x) }
  implicit def toFunction[A,B](f: A => B): Function[A,B] = Function(f)

  type Objects = Any
  type C[X,Y] = Function[X,Y]
  type →[A,B] = Function[A,B]

  def id[X] = { x: X => x }

  def compose[X,Y,Z]: (C[Y,Z], C[X,Y]) => C[X,Z] =
    (g,f) => f.raw andThen g.raw
    // (g,f) => f andThen g

}


case object ScalaProductStructure extends AnyCartesianMonoidalStructure {

  type On     = Scala.type
  implicit val on: On  = Scala

  import Scala.→

  case class pair[A,B](val _1: A, val _2: B)
  type ⊗[A,B] = pair[A,B]
  type I = Unit
  // override type ×[A,B] = pair[A,B]

  def left  [A,B]: A × B → A =
    (_:(A⊗B))._1

  def right [A,B]: A × B → B =
    (_:(A⊗B))._2

  def erase[A]: A → I =
    { a: A => () }

  def univ[A,B,X]: (X → A, X → B) => X → (A × B) =
    (f,g) => { x: X => pair(f.raw(x), g.raw(x)) }

  def assoc_right[A, B, C]: ((A × B) × C) → (A × (B × C)) =
    { x: (A × B) × C => x match { case pair(pair(a,b), c) => pair(a, pair(b,c)) } }

  def assoc_left[A, B, C]: (A × (B × C)) → ((A × B) × C) =
    { x: (A × (B × C)) => x match { case pair(a, pair(b,c)) => pair(pair(a,b), c) } }
}

import Scala.{ Function, → }

case object ListFunctor extends Functor(Scala, Scala) {

  type F[X] = Seq[X]

  def apply[X,Y](f: X → Y): F[X] → F[Y] =
    Scala.Function(_.map(f.raw))
}

case object ListMonad extends MonadOn(Scala)(ListFunctor) {

  // TODO add it to the companion of kleisli category, with generic types
  implicit def adfdafa(
    implicit klecoprod: KleisliCoproducts[ListFunctor.type,this.type]
  ): KleisliCategory[Scala.type,ListFunctor.type,this.type] = klecoprod.on

  type μ  = multiplication.type
  val μ   = multiplication

  case object multiplication extends NaturalTransformation(Scala,  ListFunctor >=> ListFunctor, ListFunctor, Scala) {

    def at[X]: Seq[Seq[X]] → Seq[X] =
      Scala.Function(_.flatten)
  }

  type η  = unit.type
  val η   = unit

  case object unit extends NaturalTransformation(Scala, IdentityFunctor(Scala), ListFunctor, Scala) {

    def at[X]: X → Seq[X] =
      Scala.Function(Seq(_))
  }
}


case object ScalaSumStructure extends AnyCocartesianMonoidalStructure {

  type On     = Scala.type
  val on: On  = Scala

  type ⊗[X, Y] = X Either Y
  type I = Nothing

  def left  [A <: On#Objects, B <: On#Objects]: A → (A + B) =
    { a: A => Left(a): A + B }

  def right [A <: On#Objects, B <: On#Objects]: B → (A + B) =
    { b: B => Right(b): A + B }

  def nothing [A <: On#Objects]: Nothing → A =
    Function[Nothing,A] { _: Nothing => ??? }

  def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]: (A → X, B → X) => ((A + B) → X) =
    (f: A → X, g: B → X) => Function { ab: (A + B) => ab.fold(f.raw,g.raw) }

  def assoc_right[A, B, C]: ((A + B) + C) → (A + (B + C)) =
    {
      ab_c: ((A + B) + C) => ab_c match {

        case Left(ab) => ab match {

          case Left(a)  => Left(a)
          case Right(b) => Right(Left(b))
        }

        case Right(c) => Right(Right(c))
      }
    }

  def assoc_left[A, B, C]: (A + (B + C)) → ((A + B) + C) =
    {
      a_bc: (A + (B + C)) => a_bc match {

        case Left(a) => Left(Left(a))

        case Right(bc) => bc match {

          case Left(b)  => Left(Right(b))
          case Right(c) => Right(c)
        }
      }
    }
}

case class KleisliCoproducts[
  F0 <: AnyFunctor { type Source = Scala.type; type Target = Scala.type },
  M <: AnyMonad { type On = Scala.type; type Functor = F0 }
]
(val m: M) extends AnyCocartesianMonoidalStructure {

  type On = KleisliCategory[Scala.type,F0,M]
  val on: On = KleisliCategory(m)

  val asM = on.freeF

  type ⊗[A,B] = ScalaSumStructure.+[A,B]

  type I = Nothing

  def left[A <: On#Objects, B <: On#Objects]: A → F0#F[A + B] =
    asM(ScalaSumStructure.left)

  def right [A <: On#Objects, B <: On#Objects]: B → F0#F[A + B] =
    asM(ScalaSumStructure.right)

  def nothing [A <: On#Objects]: I → F0#F[A] =
    asM[Nothing,A](ScalaSumStructure.nothing[A])

  def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]: (A → F0#F[X], B → F0#F[X]) => ((A + B) → F0#F[X]) =
    (a,b) => ScalaSumStructure.univ(a, b)

  def assoc_left[X <: On#Objects, Y <: On#Objects, Z <: On#Objects]: (X + (Y + Z)) → F0#F[((X + Y) + Z)] = ???

  def assoc_right[X <: On#Objects, Y <: On#Objects, Z <: On#Objects]: ((X + Y) + Z) → F0#F[(X + (Y + Z))] = ???
}





















class MealyTests extends FunSuite {

  test("local syntax") {

    val m = mealy( (x: String, u: Int) => if(x.isEmpty) (u, x) else (u + 1, x drop 1) ) :<: MealyCat
    val n = mealy( (x: String, b: Boolean) => if(b) (false, x) else (true, x drop 1)  ) :<: MealyCat

    val mm = imply(MealyCat) { m >=> n }

    // Scala std cat
    val f = { x: String => x.length } :<: Scala
    val g = { n: Int => if(n % 2 == 0) true else false } :<: Scala
    val h = { x: Boolean => if(x) 1 else 0 } :<: Scala
    val k = { x: String => if(x.length == 0) true else false }

    val fg = imply(Scala){ f >=> g }

    val fg0 = imply(Scala) {

      { x: String => x.length } :<: Scala >=> { n: Int => if(n % 2 == 0) true else false }
    }
    val fg0_again = imply(Scala) {

      ({ x: String => x.length } :<: Scala) >=> { n: Int => if(n % 2 == 0) true else false }
    }

    val z = imply(ScalaProductStructure) {

      val uhoh1 = (f × f) >=> (g × g)
      val uhoh2 = (f × f) >=> (g × g)
      uhoh2
    }

    val productMap = imply(ScalaProductStructure) { f & k }

    // Id monad on Scala
    val kleisliId = IdentityMonad(Scala).kleisliCategory
    val fg2 = imply(kleisliId) { f :<: kleisliId >=> g }

    // List/Seq monad on Scala
    val kleisliList = ListMonad.kleisliCategory
    val kleisliListCoprod = KleisliCoproducts[ListFunctor.type, ListMonad.type](ListMonad)
    val a = Function { x: String => x.toSeq } :<: kleisliList
    val b = Function { x: Char => Seq(x) }
    val c = Function { b: Boolean => if(b) Seq('t') else Seq('f') } :<: kleisliList

    imply(kleisliList) {

      val zzz = (a >=> b)
      println { zzz("hola scalac") }
    }

    imply(kleisliListCoprod) {

      val uuuh = a | c

      val uhoh = a + c

      val zz = a >=> b

      assert { uuuh(Right[String,Boolean](true)) == uuuh(Left[String,Boolean]("t")) }
    }

  }
}
