package ohnosequences.stuff

object tuples extends Product {

  lazy val monoidalCategory: Product.CartesianMonoidalCategory[this.type] =
    Product monoidalCategory (this: Product.is[this.type])

  type On = Scala
  val on = Scala

  type ×[X <: On#Objects, Y <: On#Objects] =
    ohnosequences.stuff.×[X, Y]

  type ∗ =
    EmptyTuple.type

  @inline
  final def left[A, B]: A × B -> A =
    _.left

  @inline
  final def right[A, B]: A × B -> B =
    _.right

  @inline
  final def erase[A]: A -> ∗ = { _ =>
    EmptyTuple
  }

  @inline
  final def both[X <: On#Objects, A <: On#Objects, B <: On#Objects]
    : (X -> A) × (X -> B) -> (X -> (A × B)) = { fg =>
    { x =>
      new TupleImpl(fg.left(x), fg.right(x))
    }
  }
}

private[stuff] sealed abstract class Tuple {

  type Left
  val left: Left

  type Right
  val right: Right
}

private[stuff] object EmptyTuple {

  @inline @java.lang.SuppressWarnings(
    scala.Array("org.wartremover.warts.IsInstanceOf"))
  override final def equals(other: scala.Any): scala.Boolean =
    other.isInstanceOf[this.type]
}

private[stuff] final class TupleImpl[A, B](val left: A, val right: B)
    extends Tuple {

  @inline @java.lang.SuppressWarnings(
    scala.Array("org.wartremover.warts.AsInstanceOf",
                "org.wartremover.warts.IsInstanceOf"))
  override final def equals(other: scala.Any): scala.Boolean =
    if (other.isInstanceOf[TupleImpl[A, B]]) {

      val asTuple = other.asInstanceOf[TupleImpl[A, B]]

      asTuple.left == left && asTuple.right == right
    } else false

  type Left  = A
  type Right = B
}

object Tuple {

  @inline
  final def πL[AB <: Tuple]: AB -> AB#Left =
    _.left

  @inline
  final def πR[AB <: Tuple]: AB -> AB#Right =
    _.right

  final class Syntax[A](val a: A) extends CompileTime {

    @inline
    final def and[B](b: B): A × B =
      new TupleImpl(a, b)
  }
}
