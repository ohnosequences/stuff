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

  /**
    The type of functions from A to B.
    @group functions
  */
  @infix
  type ->[A,B] =
    Function[A,B]

  /** @group products */
  type ∗ =
    EmptyTuple.type
  /** @group products */
  @infix
  type ×[A,B] =
    TupleImpl[A,B]

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
  /** @group annotations */
  type infix  =
    scala.annotation.showAsInfix
}
