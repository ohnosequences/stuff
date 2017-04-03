package ohnosequences.stuff

import scala.inline

sealed abstract class Tuple {

  type Left
  val left: Left

  type Right
  val right: Right
}

case object ∗

private[stuff] final
case class tuple[A,B](val left: A, val right: B) extends Tuple {

  type Left = A
  type Right = B
}

/*
  This has all methods needed for a Product structure.
*/
case object products {

  import Function._

  type ×[A,B] = Tuple { type Left = A; type Right = B }

  type ∗    = ohnosequences.stuff.∗.type
  val ∗ : ∗ = ohnosequences.stuff.∗

  @inline final
  def left[A,B]: A × B -> A =
    Function { _.left }

  @inline final
  def right[A,B]: A × B -> B =
    Function { _.right }

  @inline final
  def erase[A]: A -> ∗ =
    Function { a => ∗ }

  @inline final
  def duplicate[Z]: Z -> (Z × Z) =
    both(identity & identity)

  @inline final
  def swap[A,B]: (A × B) -> (B × A) =
    both(right & left)

  @inline final
  def map[A,B,C,D]: ((A -> B) × (C -> D)) -> ((A × C) -> (B × D)) =
    Function { fg =>
      both { (left >-> fg.left) & (right >-> fg.right) }
    }

  // all these functions can be generated. Yes, I mean that: code generation.
  // see http://yefremov.net/blog/scala-code-generation/ probably using Twirl
  // the key advantage here is that we generated *methods*, not classes.
  @inline final
  def πL[AB <: Tuple]: AB -> AB#Left =
    Function { _.left }

  @inline final
  def πR[AB <: Tuple]: AB -> AB#Right =
    Function { _.right }

  @inline final
  def π_1_2[A,B]: A × B -> A =
    Function { _.left }

  @inline final
  def π_1_3[A,B,C]: A × B × C -> A =
    Function { _.left.left }

  @inline final
  def π_2_3[A,B,C]: A × B × C -> B =
    Function { _.left.right }

  @inline final
  def π_3_3[A,B,C]: A × B × C -> C =
    Function { _.right }

  @inline final
  def both[A,B,X]: ((X -> A) × (X -> B)) -> (X -> (A × B)) =
    Function { fg =>
      Function {
        x => fg.left(x) & fg.right(x)
      }
    }

  @inline final
  def both3[A,B,C,X]: ((X -> A) × (X -> B) × (X -> C)) -> (X -> (A × B × C)) =
    Function { fgh =>
      Function {
        x => π_1_3(fgh)(x) & π_2_3(fgh)(x) & π_3_3(fgh)(x)
      }
    }

  implicit final
  class ProductOps[A](val a: A) extends scala.AnyVal {

    // TODO find a better, less confusing syntax for building tuples
    @inline final
    def &[B](b: B): A × B =
      tuple(a,b)
  }
}
