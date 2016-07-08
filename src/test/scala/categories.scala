package ohnosequences.stuff.test

import org.scalatest.FunSuite

import ohnosequences.stuff._

sealed trait AnyOption extends Any {

  // see http://jnordenberg.blogspot.com.es/2013/03/a-more-efficient-option.html
  import AnyOption._

  type Elem
  type F[Z] = some[Z]

  protected def isEmpty: Boolean = value == null
  protected def value: Elem

  // def apply[X,Y](f: X => Y): F[X] => F[Y]

  def flatMap[U](f: Elem => Option[U]): Option[U] = if (isEmpty) None[U] else f(value)

  def filter(p: Elem => Boolean): Option[Elem] = if (isEmpty || p(value)) some(value) else None[Elem]
  def withFilter(f: Elem => Boolean): Option[Elem] = filter(f)

  def map[U](f: Elem => U): Option[U] = if (isEmpty) None else some( f(value) )
}

case object AnyOption {

  type Option[+X] = some[X] //  AnyOption { type Elem = X }

  def None[X]: Option[X] = some[X](null.asInstanceOf[X])
  def Some[X](x: X): Option[X] = if(x == null) None else some(x)
}

case class some[+E](val value: E) extends AnyVal with AnyOption {

  type Elem = E @annotation.unchecked.uncheckedVariance
}

case object argh {

  def buh(xopt: AnyOption.Option[String]) = xopt match {

    case some(x) => println { x }
    case none => println { "nothing to see here" }
  }
}

case object Scala extends AnyCategory {

  type Objects  = Any
  type C[X,Y]   = X => Y

  final def id[X <: Objects]: C[X,X] =
    { x: X => x }

  final def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] =
    (g, f) => f andThen g
}

case object ListF extends Functor(Scala, Scala) {

  type F[X] = List[X]

  def apply[X,Y](f: X => Y): List[X] => List[Y] =
    { xs: List[X] => xs map f }
}

case object ListM extends MonadOn(Scala)(ListF) {

  type μ = mu.type
  val μ = mu
  case object mu extends NaturalTransformation(Scala,  ListF >=> ListF, ListF, Scala) {

    def at[X]: List[List[X]] => List[X] =
      _.flatten
  }

  type η = unit.type
  val η = unit
  case object unit extends NaturalTransformation(Scala, Scala.Id, ListF, Scala) {

    def at[X]: X => List[X] =
      List(_)
  }
}

case object ListFM extends LaxMonoidalFunctor(ScalaProduct, ListF, ScalaProduct) {

  import ScalaProduct._

  def zip[A,B]: List[A] × List[B] => List[A × B] =
    { case (as, bs) => as zip bs }

  def unit: Unit => List[Unit] =
    _ => List( () )
}

case object ScalaProduct extends AnyCartesianMonoidalStructure {

  type On = Scala.type
  val on: On = Scala

  type ⊗[X, Y] = (X,Y)

  type I = Unit

  def left[A,B]: A × B => A = _._1
  def right[A,B]: A × B => B = _._2

  def erase[A]: A => I = a => ()

  def univ[A,B,X]: (X => A, X => B) => X => (A × B) =
    (f,g) => { x => (f(x), g(x)) }

  def assoc_right[A, B, C]: (A × B) × C => A × (B × C) =
    { case ((a,b), c) => (a, (b,c)) }

  def assoc_left[A, B, C]: A × (B × C) => (A × B) × C =
    { case (a, (b,c)) => ((a,b), c) }
}

case object ScalaSum extends AnyCocartesianMonoidalStructure {

  type On = Scala.type
  val on: On = Scala

  type ⊗[X, Y] = X Either Y

  type I = Nothing

  def left  [A <: On#Objects, B <: On#Objects]: A => A + B =
    a => Left(a)

  def right [A <: On#Objects, B <: On#Objects]: B => A + B =
    b => Right(b)

  def nothing [A <: On#Objects]: Nothing => A =
    { _ => ??? }

  def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]: (A => X, B => X) => (A + B => X) =
    (f,g) => { ab => ab.fold(f,g) }

  def assoc_right[A, B, C]: (A + B) + C => A + (B + C) =
    {
      ab_c => ab_c match {

        case Left(ab) => ab match {

          case Left(a) => Left(a)
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

          case Left(b) => Left(Right(b))
          case Right(c) => Right(c)
        }
      }
    }
}

case object ScalaDistributiveCat extends AnyRigStructure {

  type On = Scala.type

  type Plus = ScalaSum.type
  type Mult = ScalaProduct.type

  import ScalaSum.+
  import ScalaProduct.×

  def distribute[A <: On#Objects, B <: On#Objects, C <: On#Objects]: (A + B) × C => (A × C) + (B × C) =
    {
      case (a_or_b, c) => a_or_b match {
        case Left(a)  => Left((a,c))
        case Right(b) => Right((b,c))
      }
    }

  def distribute_inv[A <: On#Objects, B <: On#Objects, C <: On#Objects]: (A × C) + (B × C) => (A + B) × C =
    {
      ac_or_bc => ac_or_bc match {

        case Left((a,c))  => (Left(a), c)
        case Right((b,c)) => (Right(b), c)
      }
    }
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

class ScalaCategoryTest extends FunSuite {

  test("Syntax for Scala category") {

    val l = Scala.id[String] >=> { x: String => x.length }

    val f = { x: Int => x.toString }

    val www = (f: Scala.C[Int,String]) >=> Scala.id[String] >=> { x: String => x.length }
  }

  test("Functors on Scala") {

    val f: Scala.C[Int,String] = { x: Int => x.toString }

    assert { Scala.Id(f) === f }

    val IdTwice = new FunctorComposition(Scala.Id, Scala.Id)

    assert { IdTwice(f) === f }
  }

  test("monads and kleisli categories") {

    val idMonad     = IdentityMonad(Scala)
    val IdKleisli   = idMonad.kleisliCategory
    val IdKleisliF  = IdKleisli.freeF // from Scala to kleisli cat of Id
    val f: Scala.C[String,Int] = { x: String => x.length } // NOTE why the types are needed here?
    assert { IdKleisliF(f)("hola") === f("hola") }

    val ListKleisli   = ListM.kleisliCategory
    val ListKleisliF  = ListKleisli.freeF
    val ListKleisliU  = ListKleisli.forgetfulF

    implicit def asC[X,Y](f: X => Y): Scala.C[X,Y] = f

    val g: ListKleisli.C[String,Char] = { xs: String => xs.toList }

    println { ListKleisliF(f)("hola scalac") }
    println { ListKleisliU[String,Char](g)(List("hola", "scalac")) }
  }

  test("monoidal structures") {

    import ScalaProduct._
    import AnyMonoidalStructure._

    val f: Scala.C[String, Int]  = { x: String => x.length }
    val g: Scala.C[Int, Boolean] = { x: Int => (x % 2) == 0 }

    val fg = f ⊗ g

    val (a,b) = fg( ("hola", 12) )
  }

  test("funny option") {

    import AnyOption._

    argh.buh(some("hola"))

    argh.buh(None)
  }

  test("Option memory usage") {

    import AnyOption._

    def calc(o1: Option[Int], o2: Option[Int]): Option[Int] =
      for {
        a <- o1
        if a > 5
        b <- o2
      } yield a + b

    def usedMem = {
      System.gc
      System.gc
      System.gc
      Runtime.getRuntime().totalMemory() - Runtime.getRuntime().freeMemory()
    }

    def report(t: String, a: => Any): Any = {
      val m1 = usedMem
      val v = a
      val m2 = usedMem
      println(t + ": " + (m2 - m1) / 1024 / 1024 + " MB")
      v
    }

    def foo = None[Int]

    def foo2 = {
      val oo1 = foo
      val i: Option[Int] = oo1
      println(i)
    }

    case object Obj
    case class Test1(o: Option[Obj.type])
    case class Test2(o: scala.Option[Obj.type])
    case class Test3(a: Any)

    foo2
    val o1 = some(10)//.some
    val oo1 = None
    val o4: Option[Any] = o1.map[Any](x => x:Any)
    val o2 = some(4)//.some
    val o3 = calc(o1, o2)
    println(o3)
    println(o1 == o1)
    println(o1 == o2)
    println(o1 == some(1))
    println(o1 == o4)

    o3 match {
      case some(x) => println("Some(" + x + ")")
      case none    => println("Nothing")
    }

    val range = 0 until 1000000
    report("scala.Some", range map (i => Test2(scala.Some(Obj))))
    report("scala.None", range map (i => Test2(scala.None)))
    report("scala.Some : Any", range map (i => Test3(scala.Some(Obj))))
    report("scala.None : Any", range map (i => Test3(scala.None)))
    report( "AnyVal Some", range map ( i => Test1(some(Obj)) ) )
    report("AnyVal None", range map (i => Test1(None)))
    report("AnyVal Some : Any", range map (i => Test3(some(Obj))))
    report("AnyVal None : Any", range map (i => Test3(None)))
  }
}
