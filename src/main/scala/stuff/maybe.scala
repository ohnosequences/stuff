package ohnosequences.stuff

import sums._

case object maybe {

  type Maybe[X] =
    ∗ + X

  type MaybeFunctor =
    Functor {
      type Source = Scala
      type Target = Scala
      type F[X] = Maybe[X]
    }

  lazy val functor: MaybeFunctor =
    +-
}
