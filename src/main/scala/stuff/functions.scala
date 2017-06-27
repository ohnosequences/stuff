package ohnosequences.stuff

import products._, functions._

object functions {

  // functions
  type ->[A,B] =
    FunctionImpl[A,B]

  @inline final
  def λ[A,B](f: A => B): A -> B =
    new FunctionImpl(f)

  @inline final
  def const[Y,X]: X -> (Y -> X) =
    λ { x: X => λ { y: Y => x } }

  @inline final
  def identity[A]: A -> A =
    λ { a: A => a }

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
    (point[A] × identity[A -> B]) >-> Scala.composition >-> force // it's nice that this works too
    // λ { af => right(af) at left(af) }

  // final
  // def coev[A,B]: B -> (A -> (A × B)) =
  //   λ { b => both(identity and (const at b)) }

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
final
class FunctionImpl[X,Y](val f: X => Y) extends scala.AnyVal {

  @inline final
  def at(d: X): Y =
    f.apply(d)

  @inline final
  def apply(a: X): Y =
    this at a

  final
  def >->[C](g: Y -> C): X -> C =
    new FunctionImpl(this.f andThen g.f)
}

object FunctionImpl {

  final implicit
  def functionProductSyntax[A,B](x: A -> B): functions.FunctionProductSyntax[A,B] =
    new functions.FunctionProductSyntax(x.f)
}
