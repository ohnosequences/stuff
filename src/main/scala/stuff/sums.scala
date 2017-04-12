package ohnosequences.stuff

import scala.{ Any, AnyVal }
import functions._
import products._
/*
  `Or` is a more reasonable sum type. Right now it is implemented using value classes for constructors; sadly, these will box (I think) in a lot of cases. A totally unboxed representation using type lists and unboxed denotations could be considered at some point.
*/
sealed trait Or extends Any {

  type Left
  type Right
  type Value

  def value: Value
}

private final
case class Left[L,R](val value: L) extends AnyVal with Or {

  type Left   = L
  type Right  = R
  type Value  = Left
}

private final
case class Right[L,R](val value: R) extends AnyVal with Or {

  type Left   = L
  type Right  = R
  type Value  = Right
}

case object sums {

  type +[A, B] =
    Or { type Left = A; type Right = B }

  type ∅

  @inline final
  def inL[A,B]: A -> (A + B) =
    λ { Left(_) }

  @inline final
  def ιL[O <: Or]: O#Left -> (O#Left + O#Right) =
    λ { Left(_) }

  @inline final
  def inR[A,B]: B -> (A + B) =
    λ { Right(_) }

  @inline final
  def ιR[O <: Or]: O#Right -> (O#Left + O#Right) =
    λ { Right(_) }

  @inline final
  def nothing[X]: ∅ -> X =
    λ { n: ∅ => scala.sys.error("∅"): X }

  @inline final
  def either[A,B,X]: ((A -> X) × (B -> X)) -> ((A + B) -> X) =
    λ {
      fg: (A -> X) × (B -> X) => λ {
        aorb: A + B =>
          if(aorb.isInstanceOf[Left[_,_]])
            fg.left(aorb.value.asInstanceOf[A])
          else
            fg.right(aorb.value.asInstanceOf[B])
      }
    }

  @inline final
  def any[A]: (A + A) -> A =
    either(identity and identity)

  @inline final
  def swap[A,B]: (A + B) -> (B + A) =
    either(inR and inL)

  @inline final
  def map[A,B,C,D]: ((A -> B) × (C -> D)) -> ((A + C) -> (B + D)) =
    λ { fg =>
      either { (fg.left >-> inL[B,D]) and (fg.right >-> inR[B,D]) }
    }

  case object SumFunctor extends Functor {

    type S = Scala.type
    val S: S = Scala

    type Source = Category.Product[S,S]
    val source: Source = Category.product(S,S)

    type Target = S
    val target = S

    type F[Z <: Source#Objects] = Z#Left + Z#Right

    def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      sums.map
  }

  @inline final implicit
  def functionSumSyntax[A,B](asdf: A -> B): FunctionSumSyntax[A,B] =
    new FunctionSumSyntax(asdf.f)

  final
  class FunctionSumSyntax[A,B](val f: A => B) extends scala.AnyVal {

    @inline final
    def +[C,D](g: C -> D): (A + C) -> (B + D) =
      map(λ(f) and g)
  }
}
