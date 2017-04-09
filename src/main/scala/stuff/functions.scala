package ohnosequences.stuff

/*
  Is there something in

  - https://adriaanm.github.io/reveal.js/scala-2.12.html
  - http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/T2_Rytz_Backend_Optimizer.pdf

  justifying a different approach?
*/
import scala.inline
import functions._
import products._

sealed abstract class Function { fn =>

  type Domain
  type Codomain

  def apply(d: Domain): Codomain

  @inline final
  def at(d: Domain): Codomain =
    apply(d)

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

object functions {

  @inline final implicit
  def fromScalaFunction[A,B](f: A => B): A -> B =
    new Function {

      type Domain   = A
      type Codomain = B

      @inline final
      def apply(a: Domain): Codomain =
        f.apply(a)
    }

  @inline final
  def λ[A,B](f: A => B): A -> B =
    fromScalaFunction { f }

  @inline final
  def const[Y,X]: X -> (Y -> X) =
    λ { x: X =>
      new Function {

        type Domain   = Y
        type Codomain = X

        @inline final
        def apply(y: Y) =
          x
      }
    }

  // NOTE what inline is doing here?
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

  /* Cartesian-closed structure */
  @inline final
  def point[X]: X -> (∗ -> X) =
    λ { x => λ { _ => x } }

  @inline final
  def force[X]: (∗ -> X) -> X =
    λ { _ at ∗ }

  @inline final
  def η[A,X,Y]: ((A × X) -> Y) -> (A -> (X -> Y)) =
    λ { f => λ { a => λ { x => f at (a and x) } } }

  @inline final
  def ϵ[A,X,Y]: (A -> (X -> Y)) -> ((A × X) -> Y) =
    λ { f => λ { ax => f at left(ax) at right(ax) } }

  @inline final
  def ev[A,B]: (A × (A -> B)) -> B =
    // (point[A] × identity[A -> B]) >-> composition >-> force // it's nice that this works too
    λ { af => right(af) at left(af) }

  @inline final
  def coev[A,B]: B -> (A -> (A × B)) =
    λ { b => both(identity and const(b)) }

  implicit final
  class FunctionProductSyntax[A,B](val f: A -> B) extends scala.AnyVal {

    @inline final
    def ×[C,D](g: C -> D): (A × C) -> (B × D) =
      map(Tuple.×(f,g))
  }
}
