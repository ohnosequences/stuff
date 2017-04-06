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
    either { λ { s: ∗ => true } & λ { s: ∗ => false } }

  def eq[A]: A × A -> Ω =
    λ { as: A × A => left(as) == right(as) } >-> fromBoolean

  val ∧ : Ω × Ω -> Ω =
    ???

  val ∨ : Ω × Ω -> Ω =
    ???
}
