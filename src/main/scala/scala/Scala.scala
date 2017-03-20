package ohnosequences.stuff

// import ohnosequences.stuff.AnyCategory
import AnyFunction._
import Product._
import scala.inline

case object Scala { // extends AnyCategory { // TODO needs updating AnyCategory

  type Objects = scala.Any

  type C[X,Y] = X --> Y

  @inline
  final def id[X <: Objects]: C[X,X] =
    identity[X]

  @inline
  def compose[X <: Objects, Y <: Objects, Z <: Objects]: C[X,Y] × C[Y,Z] --> C[X,Z] =
    function { fg => fg.left >=> fg.right }
}
