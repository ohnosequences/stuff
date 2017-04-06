package ohnosequences.stuff

import scala.inline

import products._, functions._

case object cartesianClosed {

  def point[X]: X -> (∗ -> X) =
    λ { x => λ { _ => x } }

  def force[X]: (∗ -> X) -> X =
    λ { _ at ∗ }

  def η[A,X,Y]: ((A × X) -> Y) -> (A -> (X -> Y)) =
    λ { f => λ { a => λ { x => f at (a and x) } } }

  def ϵ[A,X,Y]: (A -> (X -> Y)) -> ((A × X) -> Y) =
    λ { f => λ { ax => f at left(ax) at right(ax) } }

  import Scala._

  @inline final
  def ev[A,B]: (A × (A -> B)) -> B =
    // (point[A] × identity[A -> B]) >-> composition >-> force // it's nice that this works too
    λ { af => right(af) at left(af) }

  @inline final
  def coev[A,B]: B -> (A -> (A × B)) =
    λ { b => both(identity and const(b)) }
}
