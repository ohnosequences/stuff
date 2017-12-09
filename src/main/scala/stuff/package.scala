package ohnosequences

/** Useful stuff.

  This library is a complete reworking of Scala within Scala.

  @groupprio functions 0
  @groupname functions Functions
  @groupdesc functions Types corresponding to functions, the morphisms in the [[Scala]] category. For functions working with functions, including [[scala.Function1]] interop, see [[functions]].

  @groupprio products 1
  @groupname products Products
  @groupdesc products Types corresponding to the product structure on [[Scala]]. For functions working with products see [[products]].

  @groupprio sums 2
  @groupname sums Sums
  @groupdesc sums Types corresponding to the sum (coproduct) structure on [[Scala]]. For functions working with sums see [[sums]].

  @groupprio annotations 3
  @groupname annotations Annotations
  @groupdesc annotations Scala annotations.
  */
package object stuff {

  type Scala =
    ohnosequences.stuff.Scala.type

  /**
    The type of functions from A to B.
    @group functions
    */
  @infix
  type ->[A, B] =
    Function[A, B]

  /**
    construct functions from lambdas.

    This method for building a function out of [[scala.Function1]] instances. Apart from interoperability with other Scala code, it lets you use lambda syntax for functions:

    {{{ λ { x: String => x.length } }}}

    @group functions
    */
  @inline final def λ[A, B](f: A => B): A -> B =
    new Function(f)

  /** @group products */
  type ∗ =
    EmptyTuple.type

  val ∗ : ∗ =
    EmptyTuple

  /** @group products */
  @infix
  type ×[A, B] =
    Tuple { type Left = A; type Right = B }

  /** @group products */
  @inline
  final implicit def tupleOps[A](a: A): Tuple.Syntax[A] =
    new Tuple.Syntax(a)

  /** @group sums */
  type ∅ =
    empty.type

  /** @group sums */
  @infix
  type +[A, B] =
    Or { type Left = A; type Right = B }

  /** @group annotations */
  type inline =
    scala.inline

  type CompileTime =
    scala.AnyVal

  /** @group annotations */
  type infix =
    scala.annotation.showAsInfix

  @infix
  type <:<[A, B] =
    scala.Predef.<:<[A, B]
}
