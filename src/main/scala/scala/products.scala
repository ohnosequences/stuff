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

  import AnyFunction._

  type ×[A,B] = AnyProduct { type Left = A; type Right = B }
  type I = scala.Unit

  @inline final
  def left[A,B]: A × B --> A =
    function { _.left }

  @inline final
  def πL[A,B]: A × B --> A =
    left[A,B]

  @inline final
  def right[A,B]: A × B --> B =
    function { _.right }

  @inline final
  def πR[A,B]: A × B --> B =
    right[A,B]

  @inline final
  def both[A,B,X]: ((X --> A) × (X --> B)) --> (X --> (A × B)) =
    function { fg =>
      function {
        x => fg.left(x) × fg.right(x)
      }
    }

  @inline final
  def erase[A]: A --> I =
    function { a => () }

  @inline final
  def duplicate[Z]: Z --> (Z × Z) =
    both(identity × identity)

  @inline final
  def swap[A,B]: (A × B) --> (B × A) =
    both(right × left)

  @inline final
  def map[A,B,C,D]: ((A --> B) × (C --> D)) --> ((A × C) --> (B × D)) =
    function { fg =>
      both { (left >=> fg.left) × (right >=> fg.right) }
    }
    
  implicit final
  class ProductOps[A](val a: A) extends scala.AnyVal {

    @inline final
    def ×[B](b: B): A × B =
      Product(a,b)

    @inline final
    def :×:[B](b: B): B × A =
      Product(b,a)
  }

}
