package ohnosequences.stuff

/*
  Is there something in https://adriaanm.github.io/reveal.js/scala-2.12.html or http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/T2_Rytz_Backend_Optimizer.pdf justifying a different approach?
*/
import scala.inline
import AnyFunction._

sealed abstract class AnyFunction { fn =>

  type Domain
  type Codomain

  def apply(d: Domain): Codomain

  @inline final
  def >=>[C](g: Codomain --> C): Domain --> C =
    new AnyFunction {

      type Domain   = fn.Domain
      type Codomain = C

      @inline final
      def apply(a: Domain): Codomain =
        g.apply(fn.apply(a))
    }
}
//
// private case class Function[A,B] private[stuff] (val f: A => B) extends AnyVal with AnyFunction {
//
//   type Domain   = A
//   type Codomain = B
//
//   @inline final
//   def apply(a: A): B =
//     f.apply(a)
// }

object AnyFunction {

  def function[A,B](f: A => B): A --> B =
    new AnyFunction {

      type Domain   = A
      type Codomain = B

      @inline final
      def apply(a: A): B =
        f.apply(a)
    }

  type -->[A,B] =
    AnyFunction { type Domain = A; type Codomain = B }

  // implicit final
  // class FunctionOps[A,B](val f: A --> B) extends AnyVal {
  //
  //   @inline final
  //   def >=>[C](g: B --> C): A --> C =
  //     new AnyFunction {
  //
  //       type Domain   = A
  //       type Codomain = C
  //
  //       @inline final
  //       def apply(a: Domain): Codomain =
  //         g.apply(f.apply(a))
  //     }
  // }

  @inline final
  def identity[A]: A --> A =
    new AnyFunction {

      type Domain   = A
      type Codomain = A

      @inline final
      def apply(a: Domain): Codomain =
        a
    }
}
