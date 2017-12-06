package ohnosequences.stuff

case object maybe {

  /** The Maybe type, sometimes called Option. */
  type Maybe[X] =
    ∗ + X

  /** Canonical functor instance on Maybe, derived from the sum structure. */
  val functor =
    Coproduct(sums) ⊢ +-
}
