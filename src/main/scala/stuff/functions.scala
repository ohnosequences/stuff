package ohnosequences.stuff

import products._

object functions {

  @inline final
  def identity[A]: A -> A =
    λ { a: A => a }

  /** Convenience method for building a function out of a Scala std `Function1` */
  @inline final
  def λ[A,B](f: A => B): A -> B =
    new Function(f)

  /** a constant function from Y to X given a value of X  */
  @inline final
  def const[Y,X]: X -> (Y -> X) =
    λ { x: X => λ { y: Y => x } }

  /* Cartesian-closed structure */
  final
  def point[X]: X -> (∗ -> X) =
    λ { x => λ { _ => x } }

  final
  def force[X]: (∗ -> X) -> X =
    λ { _ at ∗ }

  final
  def η[A,X,Y]: ((A × X) -> Y) -> (A -> (X -> Y)) =
    λ { f => λ { a => λ { x => f at (a and x) } } }

  final
  def ϵ[A,X,Y]: (A -> (X -> Y)) -> ((A × X) -> Y) =
    λ { f => λ { ax => f at (left at ax) at (right at ax) } }

  final
  def ev[A,B]: (A × (A -> B)) -> B =
    (point[A] × identity[A -> B]) >-> Scala.composition >-> force
    // λ { af => right(af) at left(af) }

  final
  def coev[A,B]: B -> (A -> (A × B)) =
    λ { b => both(identity and (const at b)) }

  final
  class FunctionProductSyntax[A,B](val f: A => B) extends scala.AnyVal {

    @inline final
    def ×[C,D](g: C -> D): (A × C) -> (B × D) =
      products.map at new TupleImpl(λ(f), g)
  }
}

/*
  Is there something in

  - https://adriaanm.github.io/reveal.js/scala-2.12.html
  - http://downloads.typesafe.com/website/presentations/ScalaDaysSF2015/T2_Rytz_Backend_Optimizer.pdf

  justifying a different approach?
*/
private[stuff]
final
class Function[X,Y](val stdF: X => Y) extends scala.AnyVal {

  @inline final
  def at(d: X): Y =
    stdF apply d

  @inline final
  def apply(a: X): Y =
    this at a

  final
  def >->[C](g: Y -> C): X -> C =
    new Function(this.stdF andThen g.stdF)
}

object Function {

  final implicit
  def functionProductSyntax[A,B](x: A -> B): functions.FunctionProductSyntax[A,B] =
    new functions.FunctionProductSyntax(x.stdF)
}
