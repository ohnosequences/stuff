
```scala
package ohnosequences.stuff

import functions._

object products {

  type ×[A,B] =
    TupleImpl[A,B]

  type ∗ =
    EmptyTuple.type
    
  @inline final
  def ∗ : ∗ =
    EmptyTuple

  @inline final
  def left[A,B]: A × B -> A =
    λ { _.left }

  @inline final
  def right[A,B]: A × B -> B =
    λ { _.right }

  @inline final
  def erase[A]: A -> ∗ =
    λ { _ => ∗ }

  @inline final
  def duplicate[Z]: Z -> (Z × Z) =
    both(identity and identity)

  @inline final
  def Δ[Z]: Z -> (Z × Z) =
    duplicate

  @inline final
  def swap[A,B]: (A × B) -> (B × A) =
    both(new TupleImpl(right,left))

  @inline final
  def map[U,V,C,D]: ((U -> V) × (C -> D)) -> ((U × C) -> (V × D)) =
    λ { fg =>
      both { new TupleImpl( left >-> fg.left, right >-> fg.right ) }
      // both { (left[U,C] >-> fg.left) and (right[U,C] >-> fg.right) }
    }

  @inline final
  def assoc_right[X,Y,Z]: ((X × Y) × Z) -> (X × (Y × Z)) =
    both(π_1_3 and both(π_2_3 and π_3_3))

  @inline final
  def assoc_left[X,Y,Z]: (X × (Y × Z)) -> ((X × Y) × Z) = {

    val pickX = left[X,(Y × Z)]
    val pickY = right[X,(Y × Z)] >-> left[Y,Z]
    val pickZ = right[X,(Y × Z)] >-> right[Y,Z]

    both(new TupleImpl(both(new TupleImpl(pickX, pickY)),pickZ))
  }

  // all these functions can be generated. Yes, I mean that: code generation.
  // see http://yefremov.net/blog/scala-code-generation/ probably using Twirl
  // the key advantage here is that we generated *methods*, not classes.
  @inline final
  def πL[AB <: Tuple]: AB -> AB#Left =
    λ { _.left }

  @inline final
  def πR[AB <: Tuple]: AB -> AB#Right =
    λ { _.right }

  @inline final
  def π_1_2[A,B]: A × B -> A =
    λ { _.left }

  @inline final
  def π_1_3[A,B,C]: A × B × C -> A =
    λ { _.left.left }

  @inline final
  def π_2_3[A,B,C]: A × B × C -> B =
    λ { _.left.right }

  @inline final
  def π_3_3[A,B,C]: A × B × C -> C =
    λ { _.right }

  @inline final
  def both[A,B,X]: ((X -> A) × (X -> B)) -> (X -> (A × B)) =
    λ { fg =>
      λ {
        x => new TupleImpl(fg.left(x), fg.right(x))
      }
    }

  @inline final
  def all2[A,B,X]: ((X -> A) × (X -> B)) -> (X -> (A × B)) =
    both

  @inline final
  def all3[A,B,C,X]: ((X -> A) × (X -> B) × (X -> C)) -> (X -> (A × B × C)) =
    λ { fgh =>
      λ {
        x => new TupleImpl( new TupleImpl( π_1_3(fgh)(x), π_2_3(fgh)(x) ), π_3_3(fgh)(x) )
        // x => π_1_3(fgh)(x) and π_2_3(fgh)(x) and π_3_3(fgh)(x)
      }
    }

  object ProductFunctor extends Functor {

    type S = Scala.type
    val S: S = Scala

    type Source = Category.Product[S,S]
    val source: Source = Category.product(S,S)

    type Target = S
    val target = S

    type F[Z <: Source#Objects] = Z#Left × Z#Right

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      products.map
  }

  // syntax
  @inline final implicit
  def productOps[A](a: A): ProductOps[A] =
    new ProductOps(a)

  @inline final
  class ProductOps[A](val a: A) extends scala.AnyVal {

    @inline final
    def and[B](b: B): A × B =
      new TupleImpl(a,b)
  }
}

sealed abstract class Tuple {

  type Left
  val left: Left

  type Right
  val right: Right
}

case object EmptyTuple

final
case class TupleImpl[A,B](val left: A, val right: B) extends Tuple {

  type Left   = A
  type Right  = B
}

```




[test/scala/tuples/stdComparison.scala]: ../../../test/scala/tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: ../../../test/scala/tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: ../../../test/scala/functors/functorExamples.scala.md
[test/scala/sums.scala]: ../../../test/scala/sums.scala.md
[test/scala/ScalaCategory.scala]: ../../../test/scala/ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: ../../../test/scala/functions/syntax.scala.md
[test/scala/categories.scala]: ../../../test/scala/categories.scala.md
[main/scala/stuff/products.scala]: products.scala.md
[main/scala/stuff/Scala.scala]: Scala.scala.md
[main/scala/stuff/package.scala]: package.scala.md
[main/scala/stuff/sums.scala]: sums.scala.md
[main/scala/stuff/boolean.scala]: boolean.scala.md
[main/scala/stuff/functors.scala]: functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: categories.scala.md
[main/scala/stuff/functions.scala]: functions.scala.md