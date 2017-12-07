package ohnosequences.stuff

import functions._

abstract class Product {

  type On <: Category
  val on: Category.is[On]

  @infix
  type ×[X <: On#Objects, Y <: On#Objects] <: On#Objects

  type ∗ <: On#Objects

  def both[
      X <: On#Objects,
      A <: On#Objects,
      B <: On#Objects,
  ]
  // TODO find a convenient aliasing convention for both ×'s
    : ohnosequences.stuff.×[On#C[X, A], On#C[X, B]] -> On#C[X, A × B]

  def erase[X <: On#Objects]: On#C[X, ∗]

  def left[A <: On#Objects, B <: On#Objects]: On#C[A × B, A]
  def right[A <: On#Objects, B <: On#Objects]: On#C[A × B, B]
}

object Product {

  type is[P <: Product] =
    P {
      type On = P#On
      // format: off
      type ×[X <: On#Objects, Y <: On#Objects] = P# ×[X, Y]
      type ∗                                       = P# ∗
      // format: on
    }

  def monoidalCategory[P <: Product](p: P)(
      implicit ev: p.type <:< is[P]): CartesianMonoidalCategory[P] =
    new CartesianMonoidalCategory(ev(p))

  @inline final def apply[
      P <: Product
  ](p: P)(implicit ev: p.type <:< is[P]): Syntax[P] =
    new Syntax(ev(p))

  final class Syntax[Prod <: Product](val prod: is[Prod]) {

    // type aliases
    /////////////////////////////////////////////////////////////////////////
    type Objects =
      Prod#On#Objects

    @infix
    type >=>[X <: Prod#On#Objects, Y <: Prod#On#Objects] =
      Prod#On#C[X, Y]

    @infix
    type ×[X <: Prod#On#Objects, Y <: Prod#On#Objects] =
      // format: off
      Prod # ×[X, Y]
      // format: on

    type I =
      // format: off
      Prod # ∗
      // format: on

    // implicits
    /////////////////////////////////////////////////////////////////////////
    @inline implicit final val _on: Category.is[Prod#On] =
      prod.on

    @inline implicit final val _prod: is[Prod] =
      prod

    @inline implicit final def categoryMorphismSyntax[X <: Objects,
                                                      Y <: Objects](
        f: X >=> Y): Category.MorphismSyntax[Prod#On, X, Y] =
      new Category.MorphismSyntax(f)

    @inline implicit final def productMorphismSyntax[X <: Objects,
                                                     Y <: Objects](
        f: X >=> Y): ProductMorphismSyntax[Prod, X, Y] =
      new ProductMorphismSyntax(f)

    // aliases and derived morphisms
    @inline final def id[X <: Objects]: X >=> X =
      prod.on.identity

    @inline final def left[A <: Objects, B <: Objects]: A × B >=> A =
      prod left

    @inline final def right[A <: Objects, B <: Objects]: A × B >=> B =
      prod right

    @inline final def erase[A <: Objects]: A >=> I =
      prod erase

    @inline final def duplicate[Z <: Objects]: Z >=> (Z × Z) =
      id ^ id

    @inline final def Δ[Z <: Objects]: Z >=> (Z × Z) =
      duplicate

    @inline final def swap[A <: Objects, B <: Objects]: (A × B) >=> (B × A) =
      right ^ left

    @inline final def components[
        X <: Objects,
        A <: Objects,
        B <: Objects,
    ]: (X >=> (A × B)) -> ohnosequences.stuff.×[X >=> A, X >=> B] =
      λ { f =>
        f >=> left and f >=> right
      }
  }

  final class ProductMorphismSyntax[
      Prod <: Product,
      X <: Prod#On#Objects,
      Y <: Prod#On#Objects
  ](val f: Prod#On#C[X, Y])
      extends scala.AnyVal {

    @inline final def ^[Z <: Prod#On#Objects](g: Prod#On#C[X, Z])(
        implicit prod: is[Prod]
    ) // format: off
    : Prod#On#C[X, Prod# ×[Y, Z]] = // format: on
    prod both (f and g)

    @inline final def ×[U <: Prod#On#Objects, V <: Prod#On#Objects](
        g: Prod#On#C[U, V])(
        implicit prod: is[Prod] // format: off
    )
    : Prod#On#C[Prod# ×[X, U], Prod# ×[Y, V]] = // format: on
    Category(prod.on) ⊢ { prod both (prod.left >=> f and prod.right >=> g) }
  }
}

final class CartesianMonoidalCategory[P <: Product](val product: Product.is[P])
    extends MonoidalCategory {

  type On = P#On
  val on = product.on

  // format: off
  @infix
  type ⊗[X <: On#Objects, Y <: On#Objects] = P# ×[X, Y]
  type I                                   = P# ∗
  // format: on

  def ⊗[
      A <: On#Objects,
      B <: On#Objects,
      C <: On#Objects,
      D <: On#Objects
  ]: On#C[A, B] × On#C[C, D] -> On#C[A ⊗ C, B ⊗ D] =
    λ { fg =>
      Product(product) ⊢ {
        left >=> fg.left ^ right >=> fg.right
      }
    }

  def assoc_right[
      A <: On#Objects,
      B <: On#Objects,
      C <: On#Objects
  ]: On#C[(A ⊗ B) ⊗ C, A ⊗ (B ⊗ C)] =
    Product(product) ⊢ {
      (left[A × B, C] >=> left) ^
        ((left[A × B, C] >=> right) ^ right)
    }

  def assoc_left[
      A <: On#Objects,
      B <: On#Objects,
      C <: On#Objects
  ]: On#C[A ⊗ (B ⊗ C), (A ⊗ B) ⊗ C] =
    Product(product) ⊢ {
      (left[A, B × C] ^ (right >=> left)) ^
        (right[A, B × C] >=> right)
    }

  def unitl[A <: On#Objects]: On#C[I ⊗ A, A] =
    product.right

  def unitr[A <: On#Objects]: On#C[A ⊗ I, A] =
    product.left
}

////////////////////////////////////////////////////////////////////////////

object products {

  // object symmetricStructure extends SymmetricStructure {

  //   type On = monoidalCategory.type
  //   val on: On = monoidalCategory

  //   def swap[X, Y]: X × Y -> (Y × X) =
  //     products.swap
  // }

  @inline final def ∗ : ∗ =
    EmptyTuple

  @inline final def left[A, B]: A × B -> A =
    λ { _.left }

  @inline final def right[A, B]: A × B -> B =
    λ { _.right }

  @inline final def erase[A]: A -> ∗ =
    λ { _ =>
      ∗
    }

  @inline final def duplicate[Z]: Z -> (Z × Z) =
    both(identity and identity)

  @inline final def Δ[Z]: Z -> (Z × Z) =
    duplicate

  @inline final def swap[A, B]: (A × B) -> (B × A) =
    both(new TupleImpl(right, left))

  @inline final def map[U, V, C, D]
    : ((U -> V) × (C -> D)) -> ((U × C) -> (V × D)) =
    λ { fg =>
      both { new TupleImpl(left >-> fg.left, right >-> fg.right) }
    // both { (left[U,C] >-> fg.left) and (right[U,C] >-> fg.right) }
    }

  @inline final def assoc_right[X, Y, Z]: ((X × Y) × Z) -> (X × (Y × Z)) =
    both(π_1_3 and both(π_2_3 and π_3_3))

  @inline final def assoc_left[X, Y, Z]: (X × (Y × Z)) -> ((X × Y) × Z) = {

    val pickX = left[X, (Y × Z)]
    val pickY = right[X, (Y × Z)] >-> left[Y, Z]
    val pickZ = right[X, (Y × Z)] >-> right[Y, Z]

    both(new TupleImpl(both(new TupleImpl(pickX, pickY)), pickZ))
  }

  // all these functions can be generated. Yes, I mean that: code generation.
  // see http://yefremov.net/blog/scala-code-generation/ probably using Twirl
  // the key advantage here is that we generated *methods*, not classes.
  @inline final def πL[AB <: Tuple]: AB -> AB#Left =
    λ { _.left }

  @inline final def πR[AB <: Tuple]: AB -> AB#Right =
    λ { _.right }

  @inline final def π_1_2[A, B]: A × B -> A =
    λ { _.left }

  @inline final def π_1_3[A, B, C]: A × B × C -> A =
    λ { _.left.left }

  @inline final def π_2_3[A, B, C]: A × B × C -> B =
    λ { _.left.right }

  @inline final def π_3_3[A, B, C]: A × B × C -> C =
    λ { _.right }

  @inline final def both[A, B, X]: ((X -> A) × (X -> B)) -> (X -> (A × B)) =
    λ { fg =>
      λ { x =>
        new TupleImpl(fg.left(x), fg.right(x))
      }
    }

  @inline final def all2[A, B, X]: ((X -> A) × (X -> B)) -> (X -> (A × B)) =
    both

  @inline final def all3[A, B, C, X]
    : ((X -> A) × (X -> B) × (X -> C)) -> (X -> (A × B × C)) =
    λ { fgh =>
      λ { x =>
        new TupleImpl(new TupleImpl(π_1_3(fgh)(x), π_2_3(fgh)(x)),
                      π_3_3(fgh)(x))
      // x => π_1_3(fgh)(x) and π_2_3(fgh)(x) and π_3_3(fgh)(x)
      }
    }

  object ProductFunctor extends Functor {

    type Source = Category.ProductCategory[Scala, Scala]
    val source = Category.product(Scala and Scala)

    type Target = Scala
    val target = Scala

    type F[Z <: Source#Objects] = Z#Left × Z#Right

    def at[X <: Source#Objects, Y <: Source#Objects]
      : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
      products.map
  }

  @inline final class ProductOps[A](val a: A) extends scala.AnyVal {

    @inline final def and[B](b: B): A × B =
      new TupleImpl(a, b)
  }
}

private[stuff] sealed abstract class Tuple {

  type Left
  val left: Left

  type Right
  val right: Right
}

private[stuff] object EmptyTuple {
  @inline override final def equals(other: scala.Any): scala.Boolean =
    if (other.isInstanceOf[this.type]) true else false
}

private[stuff] final class TupleImpl[A, B](val left: A, val right: B)
    extends Tuple {

  @inline override final def equals(other: scala.Any): scala.Boolean =
    if (other.isInstanceOf[TupleImpl[A, B]]) {

      val asTuple = other.asInstanceOf[TupleImpl[A, B]]

      asTuple.left == left && asTuple.right == right
    } else false

  type Left  = A
  type Right = B
}
