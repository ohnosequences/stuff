package ohnosequences.stuff

object tuples extends Product {

  lazy val monoidalCategory: CartesianMonoidalCategory[this.type] =
    Product monoidalCategory this

  type On = Scala
  val on = Scala

  final type ×[X <: On#Objects, Y <: On#Objects] =
    ohnosequences.stuff.×[X, Y]

  type ∗ =
    EmptyTuple.type

  @inline final def left[A, B]: A × B -> A =
    λ { _.left }

  @inline final def right[A, B]: A × B -> B =
    λ { _.right }

  @inline final def erase[A]: A -> ∗ =
    λ { _ =>
      EmptyTuple
    }

  @inline final def both[X <: On#Objects, A <: On#Objects, B <: On#Objects]
    : (X -> A) × (X -> B) -> (X -> (A × B)) =
    λ { fg =>
      λ { x =>
        new TupleImpl(fg.left(x), fg.right(x))
      }
    }
}
