package ohnosequences.stuff

import products._, sums._, functions._

case object booleans {

  import scala.Predef.???

  type Ω = (∗ + ∗)

  val ⊤ : Ω = inR(∗)
  val ⊥ : Ω = inL(∗)

  val fromBoolean: scala.Boolean -> Ω =
    λ { b: scala.Boolean => if(b) ⊤ else ⊥  }

  val toBoolean: Ω -> scala.Boolean =
    either { point(true) and point(false) }

  def eq[A]: A × A -> Ω =
    λ { as: A × A => left(as) == right(as) } >-> fromBoolean

  val ∧ : Ω × Ω -> Ω =
    ???

  val ∨ : Ω × Ω -> Ω =
    ???

  // better a curried version?
  def If[X,Y]: (X -> Ω) -> (((∗ -> Y) × (∗ -> Y)) -> (X -> Y)) =
    λ { p => λ { tf => p >-> either(tf) } }

  import scala.Int

  val isEven: Int -> Ω =
    λ { x => eq(x % 2 and 0) }

  val msg =
    point("Is Even!")

  val other =
    point("Is odd!")

  val tellMe =
    If(isEven)(msg and other)
}
