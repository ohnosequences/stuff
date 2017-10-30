package ohnosequences.stuff

import functions._

abstract class Sum {

  type On <: Category
  val on: Category.is[On]

  @infix
  type +[X <: On#Objects, Y <: On#Objects] <: On#Objects

  type ∅ <: On#Objects

  def any[
      X <: On#Objects,
      A <: On#Objects,
      B <: On#Objects,
  ]
  // TODO find a convenient aliasing convention for both +'s
    : On#C[A, X] × On#C[B, X] -> On#C[A + B, X]

  def intro[X <: On#Objects]: On#C[∅, X]

  def left[A <: On#Objects, B <: On#Objects]: On#C[A, A + B]
  def right[A <: On#Objects, B <: On#Objects]: On#C[B, A + B]

  final def components[
      X <: On#Objects,
      A <: On#Objects,
      B <: On#Objects,
  ]: On#C[A + B, X] -> ohnosequences.stuff.×[On#C[A, X], On#C[B, X]] =
    λ { f =>
      Category(on) ⊢ { (left >=> f) and (right >=> f) }
    }
}

/*
  `Or` is a more reasonable sum type. Right now it is implemented using value classes for constructors; sadly, these will box (I think) in a lot of cases. A totally unboxed representation using type lists and unboxed denotations could be considered at some point.
 */
object sums {

  final def inL[A, B]: A -> (A + B) =
    λ { new Left(_) }

  final def ιL[O <: Or]: O#Left -> (O#Left + O#Right) =
    λ { new Left(_) }

  final def inR[A, B]: B -> (A + B) =
    λ { new Right(_) }

  final def ιR[O <: Or]: O#Right -> (O#Left + O#Right) =
    λ { new Right(_) }

  final def nothing[X]: ∅ -> X =
    λ { n: ∅ =>
      scala.sys.error("∅"): X
    }

  final def either[A, B, X]: ((A -> X) × (B -> X)) -> ((A + B) -> X) =
    λ { fg: (A -> X) × (B -> X) =>
      λ { aorb: A + B =>
        if (aorb.isInstanceOf[Left[_, _]])
          fg.left(aorb.value.asInstanceOf[A])
        else
          fg.right(aorb.value.asInstanceOf[B])
      }
    }

  final def any[A]: (A + A) -> A =
    either(identity and identity)

  final def swap[A, B]: (A + B) -> (B + A) =
    either(inR and inL)

  final def map[A, B, C, D]: ((A -> B) × (C -> D)) -> ((A + C) -> (B + D)) =
    λ { fg =>
      either { (fg.left >-> inL[B, D]) and (fg.right >-> inR[B, D]) }
    }

  /** returns the "X + -" functor [[Scala]] → [[Scala]]. */
  @inline final def +-[X]: +-[X] =
    new +-

  /** returns the "- + X" functor [[Scala]] → [[Scala]]. */
  @inline final def -+[X]: -+[X] =
    new -+

  private[stuff] final class -+[X] extends Functor {

    type Source = Scala
    val source = Scala

    type Target = Scala
    val target = Scala

    type F[A] = A + X
    def at[A, B]: (A -> B) -> (F[A] -> F[B]) =
      λ { f =>
        map(f and identity)
      }
  }

  private[stuff] final class +-[X] extends Functor {

    type Source = Scala
    val source = Scala

    type Target = Scala
    val target = Scala

    type F[A] = X + A
    def at[A, B]: (A -> B) -> (F[A] -> F[B]) =
      λ { f =>
        map(identity and f)
      }
  }

  def f[X]: (scala.Predef.String + X) -> (scala.Int + X) =
    -+ at λ { _.length }

  object SumFunctor extends Functor {

    type Source = Category.ProductCategory[Scala, Scala]
    val source = Category.product(Scala and Scala)

    type Target = Scala
    val target = Scala

    type F[Z <: Source#Objects] = Z#Left + Z#Right

    final def at[X <: Source#Objects, Y <: Source#Objects]
      : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
      sums.map
  }

  final implicit def functionSumSyntax[A, B](
      asdf: A -> B): FunctionSumSyntax[A, B] =
    new FunctionSumSyntax(asdf.stdF)

  final class FunctionSumSyntax[A, B](val f: A => B) extends scala.AnyVal {

    @inline final def +[C, D](g: C -> D): (A + C) -> (B + D) =
      map(λ(f) and g)
  }
}

private[stuff] object empty

private[stuff] sealed abstract class Or extends {

  type Left
  type Right
  type Value

  def value: Value
}

private final class Left[L, R](val value: L) extends Or {

  type Left  = L
  type Right = R
  type Value = L

  override final def equals(that: scala.Any): scala.Boolean =
    that match {
      case otherLeft: ohnosequences.stuff.Left[L, R] =>
        otherLeft.value == this.value
      case _ => false
    }
}

private final class Right[L, R](val value: R) extends Or {

  type Left  = L
  type Right = R
  type Value = Right

  override final def equals(that: scala.Any): scala.Boolean =
    that match {
      case otherRight: ohnosequences.stuff.Right[L, R] =>
        otherRight.value == this.value
      case _ => false
    }
}
