package ohnosequences.stuff

import scala.{ Any, AnyVal, inline }
import Function._
import Product._
/*
  `Or` is a more reasonable sum type. Right now it is implemented using value classes for constructors; sadly, these will box (I think) in a lot of cases. A totally unboxed representation using type lists and unboxed denotations could be considered at some point.
*/
sealed trait Or extends Any {

  type Left
  type Right
  type Value

  def value: Value
}

private case class Left[L,R](val value: L) extends AnyVal with Or {

  type Left   = L
  type Right  = R
  type Value  = Left
}

private case class Right[L,R](val value: R) extends AnyVal with Or {

  type Left   = L
  type Right  = R
  type Value  = Right
}

case object Sums {

  type +[A, B] =
    Or { type Left = A; type Right = B }

  type ∅

  @inline final
  def inL[A,B]: A -> (A + B) =
    Function { Left(_) }

  @inline final
  def inR[A,B]: B -> (A + B) =
    Function { Right(_) }

  @inline final
  def nothing[X]: ∅ -> X =
    Function { n: ∅ => scala.sys.error("∅"): X }

  // TODO try to get rid of the erasure issue
  @inline final
  def either[A,B,X]: ((A -> X) × (B -> X)) -> ((A + B) -> X) =
    Function {
      fg: (A -> X) × (B -> X) => Function {
        aorb: A + B => aorb match {
          case Left(a:A)  => fg.left(a)
          case Right(b:B) => fg.right(b)
        }
      }
    }

  @inline final
  def any[A]: (A + A) -> A =
    either(identity × identity)

  @inline final
  def swap[A,B]: (A + B) -> (B + A) =
    either(inR × inL)

  @inline final
  def map[A,B,C,D]: ((A -> B) × (C -> D)) -> ((A + C) -> (B + D)) =
    Function { fg =>
      either { (fg.left >-> inL[B,D]) × (fg.right >-> inR[B,D]) }
    }

  // simpler, no scala stuff
  // def univAlt[A,B,X]: ((A -> X) × (B -> X)) -> ((A + B) -> X) =
  //   Function {
  //     fg: (A -> X) × (B -> X) => Function { aorb =>
  //       if(aorb.isInstanceOf[Left[A,B]])
  //         fg.left(aorb.value.asInstanceOf[A])
  //       else
  //         fg.right(aorb.value.asInstanceOf[B])
  //     }
  //   }
}
