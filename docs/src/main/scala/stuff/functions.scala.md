
```scala
package ohnosequences.stuff

import products._

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
    construct functions from lambdas.

    This method for building a function out of [[scala.Function1]] instances. Apart from interoperability with other Scala code, it lets you use lambda syntax for functions:

    {{{ λ { x: String => x.length } }}}

    @group basic
  */
  @inline final
  def λ[A,B](f: A => B): A -> B =
    new Function(f)

  /**
    the identity function on `A`

    @group basic
  */
  @inline final
  def identity[A]: A -> A =
    λ { a: A => a }

  /** a constant function from Y to X given a value of X  */
  @inline final
  def const[Y,X]: X -> (Y -> X) =
    λ { x: X => λ { y: Y => x } }

  /** @group ccc */
  final
  def point[X]: X -> (∗ -> X) =
    λ { x => λ { _ => x } }

  /** @group ccc */
  final
  def force[X]: (∗ -> X) -> X =
    λ { _ at ∗ }

  /** @group ccc */
  final
  def η[A,X,Y]: ((A × X) -> Y) -> (A -> (X -> Y)) =
    λ { f => λ { a => λ { x => f at (a and x) } } }

  /** @group ccc */
  final
  def ϵ[A,X,Y]: (A -> (X -> Y)) -> ((A × X) -> Y) =
    λ { f => λ { ax => f at (left at ax) at (right at ax) } }

  /** @group ccc */
  final
  def ev[A,B]: (A × (A -> B)) -> B =
    (point[A] × identity[A -> B]) >-> Scala.composition >-> force
    // λ { af => right(af) at left(af) }

  /** @group ccc */
  final
  def coev[A,B]: B -> (A -> (A × B)) =
    λ { b => both(identity and (const at b)) }

  private[stuff]
  final
  class FunctionProductSyntax[A,B](val f: A => B) extends scala.AnyVal {

    @inline final
    def ×[C,D](g: C -> D): (A × C) -> (B × D) =
      products.map at new TupleImpl(λ(f), g)
  }
}

private[stuff]
final
class Function[X,Y](val stdF: X => Y) extends scala.AnyVal {

  @inline final
  def at(d: X): Y =
    stdF apply d

  @inline final
  def apply(a: X): Y =
    this at a

  final
  def >->[C](g: Y -> C): X -> C =
    new Function(this.stdF andThen g.stdF)
}

private[stuff]
object Function {

  final implicit
  def functionProductSyntax[A,B](x: A -> B): functions.FunctionProductSyntax[A,B] =
    new functions.FunctionProductSyntax(x.stdF)
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