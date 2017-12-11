package ohnosequences.stuff

/**
  =The Scala category=

  This object is the [[Category]] corresponding to all types as objects and functions as morphisms.
  */
object Scala extends Category {

  /** Every type is a subtype of [[http://www.scala-lang.org/api/2.12.3/scala/Any.html Any]]. */
  type Objects =
    scala.Any

  /** Morphisms between `X` and `Y` are functions between them. */
  type C[X, Y] =
    X -> Y

  /** The identity function `x: X => x` */
  @inline
  final def identity[X <: Objects]: C[X, X] =
    functions.identity

  /** Function composition. */
  @inline
  final def composition[X <: Objects, Y <: Objects, Z <: Objects]
    : C[X, Y] × C[Y, Z] -> C[X, Z] =
    λ { fg =>
      fg.left >-> fg.right
    }
}
