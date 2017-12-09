package ohnosequences.stuff

/**
  =Functions on functions=

  @groupprio basic 0
  @groupname basic
  @groupdesc basic

  @groupprio ccc 1
  @groupname ccc Cartesian-closed structure
  @groupdesc ccc This methods correspond to the Cartesian-closed structure on [[Scala]].

  */
object functions {

  /**
    the identity function on `A`

    @group basic
    */
  @inline
  final def identity[A]: A -> A =
    λ { a: A =>
      a
    }

  /** a constant function from Y to X given a value of X  */
  @inline
  final def const[Y, X]: X -> (Y -> X) =
    λ { x: X =>
      λ { y: Y =>
        x
      }
    }

  @inline
  final def point[X]: X -> (∗ -> X) =
    λ { x =>
      λ { _ =>
        x
      }
    }

  @inline
  final def force[X]: (∗ -> X) -> X =
    λ { _ at ∗ }
}

private[stuff] final class Function[X, Y](val stdF: X => Y)
    extends CompileTime {

  @inline final def at(d: X): Y =
    stdF apply d

  @inline final def apply(a: X): Y =
    this at a

  final def >->[C](g: Y -> C): X -> C =
    new Function(this.stdF andThen g.stdF)
}
