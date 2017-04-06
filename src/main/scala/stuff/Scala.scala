package ohnosequences.stuff

import functions._
import products._
import scala.inline

object Scala extends Category {

  @inline final
  type Objects =
    scala.Any

  @inline final
  type C[X,Y] =
    X -> Y

  @inline final
  def identity[X <: Objects]: C[X,X] =
    identity[X]

  @inline final
  def composition[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
    λ { fg => fg.left >-> fg.right }
}
