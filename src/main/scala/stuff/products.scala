package ohnosequences.stuff

import scala.inline

sealed trait AnyProduct {

  type Left
  def left: Left

  type Right
  def right: Right
}

private[stuff] case class Product[A,B](val left: A, val right: B) extends AnyProduct {

  type Left = A
  type Right = B
}

/*
  This has all methods needed for a Product structure.
*/
case object Product {

  import Function._

  type ×[A,B] = AnyProduct { type Left = A; type Right = B }
  type I = scala.Unit

  @inline final
  def πL[AB <: AnyProduct]: AB -> AB#Left =
    Function { _.left }

  @inline final
  def πR[AB <: AnyProduct]: AB -> AB#Right =
    Function { _.right }

  @inline final
  def left[A,B]: A × B -> A =
    Function { _.left }

  @inline final
  def right[A,B]: A × B -> B =
    Function { _.right }

  @inline final
  def both[A,B,X]: ((X -> A) × (X -> B)) -> (X -> (A × B)) =
    Function { fg =>
      Function {
        x => fg.left(x) x fg.right(x)
      }
    }

  @inline final
  def erase[A]: A -> I =
    Function { a => () }

  @inline final
  def duplicate[Z]: Z -> (Z × Z) =
    both(identity x identity)

  @inline final
  def swap[A,B]: (A × B) -> (B × A) =
    both(right x left)

  @inline final
  def map[A,B,C,D]: ((A -> B) × (C -> D)) -> ((A × C) -> (B × D)) =
    Function { fg =>
      both { (left >-> fg.left) x (right >-> fg.right) }
    }

  implicit final
  class ProductOps[A](val a: A) extends scala.AnyVal {

    // TODO find a better, less confusing syntax for building tuples
    @inline final
    def x[B](b: B): A × B =
      Product(a,b)
  }

  implicit final
  class FunctionProductSyntax[A,B](val f: A -> B) extends scala.AnyVal {

    @inline final
    def ×[C,D](g: C -> D): (A × C) -> (B × D) =
      Product.map(Product(f,g))
  }

}
