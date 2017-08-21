package ohnosequences.stuff

import sums._

case object maybe {

  /** The Maybe type, sometimes called Option. */
  type Maybe[X] =
    ∗ + X

  type MaybeFunctor =
    Functor {
      type Source = Scala
      type Target = Scala
      type F[X] = Maybe[X]
    }

  /** Canonical functor instance on Maybe, derived from the sum structure. */
  val functor: MaybeFunctor =
    +-
}
