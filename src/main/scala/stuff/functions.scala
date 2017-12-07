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
  @inline final def identity[A]: A -> A =
    λ { a: A =>
      a
    }

  /** a constant function from Y to X given a value of X  */
  @inline final def const[Y, X]: X -> (Y -> X) =
    λ { x: X =>
      λ { y: Y =>
        x
      }
    }

  /** @group ccc */
  final def point[X]: X -> (∗ -> X) =
    λ { x =>
      λ { _ =>
        x
      }
    }

  /** @group ccc */
  final def force[X]: (∗ -> X) -> X =
    λ { _ at ∗ }

  /** @group ccc */
  final def η[A, X, Y]: ((A × X) -> Y) -> (A -> (X -> Y)) =
    λ { f =>
      λ { a =>
        λ { x =>
          f at (a and x)
        }
      }
    }

  // /** @group ccc */
  // final def ϵ[A, X, Y]: (A -> (X -> Y)) -> ((A × X) -> Y) =
  //   λ { f =>
  //     λ { ax =>
  //       f at (left at ax) at (right at ax)
  //     }
  //   }

  // /** @group ccc */
  // final def ev[A, B]: (A × (A -> B)) -> B =
  //   (point[A] × identity[A -> B]) >-> Scala.composition >-> force
  // // λ { af => right(af) at left(af) }

  // /** @group ccc */
  // final def coev[A, B]: B -> (A -> (A × B)) =
  //   λ { b =>
  //     both(identity and (const at b))
  //   }

  private[stuff] final class FunctionProductSyntax[A, B](val f: A => B)
      extends scala.AnyVal {

    @inline final def ×[C, D](g: C -> D): (A × C) -> (B × D) =
      products.map at new TupleImpl(λ(f), g)
  }
}

private[stuff] final class Function[X, Y](val stdF: X => Y)
    extends scala.AnyVal {

  @inline final def at(d: X): Y =
    stdF apply d

  @inline final def apply(a: X): Y =
    this at a

  final def >->[C](g: Y -> C): X -> C =
    new Function(this.stdF andThen g.stdF)
}

private[stuff] object Function {

  final implicit def functionProductSyntax[A, B](
      x: A -> B): functions.FunctionProductSyntax[A, B] =
    new functions.FunctionProductSyntax(x.stdF)
}
