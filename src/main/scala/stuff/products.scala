package ohnosequences.stuff

import scala.inline

sealed abstract class Tuple {

  type Left
  val left: Left

  type Right
  val right: Right
}

case object ∗

case object Tuple {

  final
  case class ×[A,B](val left: A, val right: B) extends Tuple {

    type Left = A
    type Right = B
  }
}

/*
  This has all methods needed for a Product structure.
*/
case object products {

  import functions._

  type ×[A,B] = Tuple { type Left = A; type Right = B }

  type ∗    = ohnosequences.stuff.∗.type

  @inline final
  def left[A,B]: A × B -> A =
    λ { _.left }

  @inline final
  def right[A,B]: A × B -> B =
    λ { _.right }

  @inline final
  def erase[A]: A -> ∗ =
    λ { a => ∗ }

  @inline final
  def duplicate[Z]: Z -> (Z × Z) =
    both(identity & identity)

  @inline final
  def Δ[Z]: Z -> (Z × Z) =
    duplicate

  @inline final
  def swap[A,B]: (A × B) -> (B × A) =
    both(right & left)

  @inline final
  def map[A,B,C,D]: ((A -> B) × (C -> D)) -> ((A × C) -> (B × D)) =
    λ { fg =>
      both { (left >-> fg.left) and (right >-> fg.right) }
    }

  // all these functions can be generated. Yes, I mean that: code generation.
  // see http://yefremov.net/blog/scala-code-generation/ probably using Twirl
  // the key advantage here is that we generated *methods*, not classes.
  @inline final
  def πL[AB <: Tuple]: AB -> AB#Left =
    λ { _.left }

  @inline final
  def πR[AB <: Tuple]: AB -> AB#Right =
    λ { _.right }

  @inline final
  def π_1_2[A,B]: A × B -> A =
    λ { _.left }

  @inline final
  def π_1_3[A,B,C]: A × B × C -> A =
    λ { _.left.left }

  @inline final
  def π_2_3[A,B,C]: A × B × C -> B =
    λ { _.left.right }

  @inline final
  def π_3_3[A,B,C]: A × B × C -> C =
    λ { _.right }

  @inline final
  def both[A,B,X]: ((X -> A) × (X -> B)) -> (X -> (A × B)) =
    λ { fg =>
      λ {
        x => fg.left(x) & fg.right(x)
      }
    }

  @inline final
  def all2[A,B,X]: ((X -> A) × (X -> B)) -> (X -> (A × B)) =
    both

  @inline final
  def all3[A,B,C,X]: ((X -> A) × (X -> B) × (X -> C)) -> (X -> (A × B × C)) =
    λ { fgh =>
      λ {
        x => π_1_3(fgh)(x) & π_2_3(fgh)(x) & π_3_3(fgh)(x)
      }
    }

  case object ProductFunctor extends Functor {

    type S = Scala.type
    val S: S = Scala

    type Source = Category.Product[S,S]
    val source: Source = Category.product(S,S)

    type Target = S
    val target = S

    type F[Z <: Source#Objects] = Z#Left × Z#Right

    def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      products.map
  }

  implicit final
  class ProductOps[A](val a: A) extends scala.AnyVal {

    @inline final
    def and[B](b: B): A × B =
      new Tuple.×(a,b)

    // TODO find a better, less confusing symbolic syntax for building tuples
    @inline final
    def &[B](b: B): A × B =
      new Tuple.×[A,B](a,b)
  }
}
