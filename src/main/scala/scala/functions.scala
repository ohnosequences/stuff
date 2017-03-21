package ohnosequences.stuff

/*
  Is there something in https://adriaanm.github.io/reveal.js/scala-2.12.html or http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/T2_Rytz_Backend_Optimizer.pdf justifying a different approach?
*/
import scala.inline
import Function._

sealed abstract class Function { fn =>

  type Domain
  type Codomain

  def apply(d: Domain): Codomain

  @inline final
  def >->[C](g: Codomain -> C): Domain -> C =
    new Function {

      type Domain   = fn.Domain
      type Codomain = C

      @inline final
      def apply(a: Domain): Codomain =
        g.apply(fn.apply(a))
    }
}

object Function {


  // TODO what is this supposed to do?
  @inline final
  type ->[A,B] =
    Function { type Domain = A; type Codomain = B }

  @inline final
  def identity[A]: A -> A =
    new Function {

      type Domain   = A
      type Codomain = A

      @inline final
      def apply(a: Domain): Codomain =
        a
    }

  @inline final
  def apply[A,B](f: A => B): A -> B =
    new Function {

      type Domain   = A
      type Codomain = B

      @inline final
      def apply(a: A): B =
        f.apply(a)
      }
}
