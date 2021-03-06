package ohnosequences.stuff

object sums extends Coproduct {

  lazy val monoidalCategory: Coproduct.CocartesianMonoidalCategory[this.type] =
    Coproduct monoidalCategory (this: Coproduct.is[this.type])

  type On = Scala
  val on = Scala

  type +[X, Y] =
    ohnosequences.stuff.+[X, Y]

  type ∅ =
    ohnosequences.stuff.∅

  @inline
  final def intro[X]: ∅ -> X = { n: ∅ =>
    scala.sys.error("∅"): X
  }

  @inline
  final def left[A, B]: A -> (A + B) =
    new Left(_)

  @inline
  final def right[A, B]: B -> (A + B) =
    new Right(_)

  @inline
  final def either[A, B, X]: ((A -> X) × (B -> X)) -> ((A + B) -> X) = {
    fg: (A -> X) × (B -> X) =>
      { aorb: A + B =>
        @java.lang.SuppressWarnings(
          scala.Array("org.wartremover.warts.AsInstanceOf",
                      "org.wartremover.warts.IsInstanceOf"))
        val x =
          if (aorb.isInstanceOf[Left[_, _]])
            fg.left(aorb.value.asInstanceOf[A])
          else
            fg.right(aorb.value.asInstanceOf[B])

        x
      }
  }

  @inline
  final def ιL[O <: Or]: O#Left -> (O#Left + O#Right) =
    new Left(_)

  @inline
  final def ιR[O <: Or]: O#Right -> (O#Left + O#Right) =
    new Right(_)

  @inline
  final def inL[A, B]: A -> (A + B) =
    left

  @inline
  final def inR[A, B]: B -> (A + B) =
    right
}

/*
  `Or` is a more reasonable sum type. Right now it is implemented using value classes for constructors; sadly, these will box (I think) in a lot of cases. A totally unboxed representation using type lists and unboxed denotations could be considered at some point.
 */
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
