
```scala
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

```




[test/scala/tuples/stdComparison.scala]: ../../../test/scala/tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: ../../../test/scala/tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: ../../../test/scala/functors/functorExamples.scala.md
[test/scala/sums.scala]: ../../../test/scala/sums.scala.md
[test/scala/ScalaCategory.scala]: ../../../test/scala/ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: ../../../test/scala/functions/syntax.scala.md
[test/scala/categories.scala]: ../../../test/scala/categories.scala.md
[main/scala/stuff/monoidalCategories.scala]: monoidalCategories.scala.md
[main/scala/stuff/products.scala]: products.scala.md
[main/scala/stuff/Scala.scala]: Scala.scala.md
[main/scala/stuff/package.scala]: package.scala.md
[main/scala/stuff/sums.scala]: sums.scala.md
[main/scala/stuff/monoids.scala]: monoids.scala.md
[main/scala/stuff/maybe.scala]: maybe.scala.md
[main/scala/stuff/boolean.scala]: boolean.scala.md
[main/scala/stuff/functors.scala]: functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: categories.scala.md
[main/scala/stuff/functions.scala]: functions.scala.md