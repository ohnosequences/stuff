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
    a => a

  /** a constant function from Y to X given a value of X  */
  @inline
  final def const[Y, X]: X -> (Y -> X) =
    x => _ => x

  @inline
  final def point[X]: X -> (∗ -> X) =
    x => _ => x

  @inline
  final def force[X]: (∗ -> X) -> X =
    _(∗)
}

private[stuff] abstract class Function[X, Y] {

  def apply(a: X): Y
}

object Function {

  implicit final class Syntax[X, Y](val f: X -> Y) extends CompileTime {

    def >->[Z](g: Y -> Z): X -> Z =
      x => g(f(x))

    def at(x: X): Y =
      f(x)
  }
}
