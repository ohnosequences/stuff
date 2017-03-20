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

  @inline final def left[A,B]: A × B --> A =
    function { _.left }

  @inline final def πL[A,B]: A × B --> A =
    left[A,B]

  @inline final def right[A,B]: A × B --> B =
    function { _.right }

  @inline final def πR[A,B]: A × B --> B =
    right[A,B]

  def duplicate[Z]: Z --> (Z × Z) =
    function { a => (a × a) }

  def erase[A]: A --> I =
    function { a => () }

  def swap[A,B]: (A × B) --> (B × A) =
    function { ab: A × B => ab.right × ab.left }

  implicit final class ProductOps[A](val a: A) extends scala.AnyVal {

    def ×[B](b: B): A × B = Product(a,b)
    def :×:[B](b: B): B × A = Product(b,a)
  }

  def map[A,B,C,D]: ((A --> B) × (C --> D)) --> ((A × C) --> (B × D)) =
    function { fg =>
      function { ac =>
        Product( fg.left(ac.left), fg.right(ac.right) )
      }
    }
}
