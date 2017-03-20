package ohnosequences.stuff

/*
  Is there something in https://adriaanm.github.io/reveal.js/scala-2.12.html or http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/T2_Rytz_Backend_Optimizer.pdf justifying a different approach?
*/
import scala.{ Any, AnyVal, inline }

sealed trait AnyFunction extends Any {

  type Domain
  type Codomain

  def apply(d: Domain): Codomain
}

case class function[A,B](val f: A => B) extends AnyVal with AnyFunction {

  type Domain   = A
  type Codomain = B

  @inline final def apply(a: A): B =
    f.apply(a)
}

case object AnyFunction {

  type -->[A,B] = AnyFunction { type Domain = A; type Codomain = B }

  implicit final class FunctionOps[A,B](val f: A --> B) extends AnyVal {

    def >=>[C](g: B --> C): A --> C =
      new AnyFunction {
        type Domain = A; type Codomain = C
        @inline final def apply(a: Domain): Codomain = g.apply(f.apply(a))
      }
  }

  @inline final def identity[A]: A --> A =
    new AnyFunction {
      type Domain = A; type Codomain = A;
      @inline final def apply(a: Domain): Codomain = a
    }

  // final def coerce[A0 <: A, A]: A0 --> A =
  //   function { a0: A0 => a0: A }
}
