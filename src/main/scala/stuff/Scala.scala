package ohnosequences.stuff

import products._, functions._

object Scala extends Category {

  final
  type Objects =
    scala.Any

  final
  type C[X,Y] =
    X -> Y

  @inline final
  def identity[X <: Objects]: C[X,X] =
    functions.identity[X]

  @inline final
  def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
    λ { fg => fg.left >-> fg.right }
}
