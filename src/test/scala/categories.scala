package ohnosequences.stuff.test

import ohnosequences.stuff._
import ohnosequences.stuff.products._
import ohnosequences.stuff.functions._

// import org.scalatest.FunSuite

sealed trait AnyMealy extends scala.Any {

  type Input
  type State
  type Output

  def apply: (Input × State) -> (State × Output)
}

case class Mealy[I,S,O](val next: (I × S) -> (S × O)) extends scala.AnyVal with AnyMealy {

  type Input  = I
  type State  = S
  type Output = O

  @inline final
  def apply =
    next
}

case object Mealy {

  type between[I,O] =
    AnyMealy {
      type Input  = I
      type Output = O
    }
}

case object Machines extends Category {

  type Objects = Scala.Objects

  type C[X <: Objects, Y <: Objects] = Mealy.between[X,Y]

  @inline final
  def identity[X] =
    Mealy[X,∗,X](swap)

  @inline final
  def composition[X,Y,Z]: C[X,Y] × C[Y,Z] -> C[X,Z] =
    λ { mn =>

      val m = left(mn); val n = right(mn)

      Mealy(
        assoc_left                            >->
        (m.apply × Scala.identity[n.State])   >->
        assoc_right                           >->
        (Scala.identity[m.State] × n.apply)   >->
        assoc_left
      )
    }
}
