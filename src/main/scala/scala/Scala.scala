package ohnosequences.stuff

// import ohnosequences.stuff.AnyCategory
import Function._
import Product._
import scala.inline

case object Scala { // extends AnyCategory { // TODO needs updating AnyCategory

  type Objects =
    scala.Any

  type C[X,Y] =
    X -> Y

  @inline final
  def id[X <: Objects]: C[X,X] =
    identity[X]

  @inline final
  def compose[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] -> C[X,Z] =
    Function { fg => fg.left >=> fg.right }
}
