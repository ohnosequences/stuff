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

object sums {

  type +[A, B] =
    Or { type Left = A; type Right = B }

  type ∅ = empty.type
  object empty

  final
  def inL[A,B]: A -> (A + B) =
    λ { Left(_) }

  final
  def ιL[O <: Or]: O#Left -> (O#Left + O#Right) =
    λ { Left(_) }

  final
  def inR[A,B]: B -> (A + B) =
    λ { Right(_) }

  final
  def ιR[O <: Or]: O#Right -> (O#Left + O#Right) =
    λ { Right(_) }

  final
  def nothing[X]: ∅ -> X =
    λ { n: ∅ => scala.sys.error("∅"): X }

  final
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

  final
  def any[A]: (A + A) -> A =
    either(identity and identity)

  final
  def swap[A,B]: (A + B) -> (B + A) =
    either(inR and inL)

  final
  def map[A,B,C,D]: ((A -> B) × (C -> D)) -> ((A + C) -> (B + D)) =
    λ { fg =>
      either { (fg.left >-> inL[B,D]) and (fg.right >-> inR[B,D]) }
    }

  object SumFunctor extends Functor {

    type S = Scala.type
    final val S: S = Scala

    type Source = Category.Product[S,S]
    final val source: Source = Category.product(S,S)

    type Target = S
    final val target = S

    type F[Z <: Source#Objects] = Z#Left + Z#Right

    final
    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      sums.map
  }

  final implicit
  def functionSumSyntax[A,B](asdf: A -> B): FunctionSumSyntax[A,B] =
    new FunctionSumSyntax(asdf.f)

  final
  class FunctionSumSyntax[A,B](val f: A => B) extends scala.AnyVal {

    final
    def +[C,D](g: C -> D): (A + C) -> (B + D) =
      map(λ(f) and g)
  }
}
