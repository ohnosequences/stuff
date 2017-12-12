package ohnosequences.stuff

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
  ]: ohnosequences.stuff.×[On#C[X, A], On#C[X, B]] -> On#C[X, A × B]

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

  @inline
  final def apply[
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
    @inline
    implicit final val _on: Category.is[Prod#On] =
      prod.on

    @inline
    implicit final val _prod: is[Prod] =
      prod

    @inline
    implicit final def categoryMorphismSyntax[X <: Objects, Y <: Objects](
        f: X >=> Y): Category.MorphismSyntax[Prod#On, X, Y] =
      new Category.MorphismSyntax(f)

    @inline
    implicit final def productMorphismSyntax[X <: Objects, Y <: Objects](
        f: X >=> Y): ProductMorphismSyntax[Prod, X, Y] =
      new ProductMorphismSyntax(f)

    // aliases and derived morphisms
    @inline
    final def id[X <: Objects]: X >=> X =
      prod.on.identity

    @inline
    final def left[A <: Objects, B <: Objects]: A × B >=> A =
      prod left

    @inline
    final def right[A <: Objects, B <: Objects]: A × B >=> B =
      prod right

    @inline
    final def erase[A <: Objects]: A >=> I =
      prod erase

    @inline
    final def duplicate[Z <: Objects]: Z >=> (Z × Z) =
      id ^ id

    @inline
    final def Δ[Z <: Objects]: Z >=> (Z × Z) =
      duplicate

    @inline
    final def swap[A <: Objects, B <: Objects]: (A × B) >=> (B × A) =
      right ^ left

    @inline
    final def components[
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
      extends CompileTime {

    @inline
    final def ^[Z <: Prod#On#Objects](g: Prod#On#C[X, Z])(
        implicit prod: is[Prod]
    ) // format: off
    : Prod#On#C[X, Prod# ×[Y, Z]] = // format: on
    prod both (f and g)

    @inline
    final def ×[U <: Prod#On#Objects, V <: Prod#On#Objects](g: Prod#On#C[U, V])(
        implicit prod: is[Prod] // format: off
    )
    : Prod#On#C[Prod# ×[X, U], Prod# ×[Y, V]] = // format: on
    Category(prod.on) ⊢ { prod both (prod.left >=> f and prod.right >=> g) }
  }

  type CartesianMonoidalCategory[P <: Product] =
    CartesianMonoidalCategoryImpl {

      type On =
        P#On

      // format: off
      type ⊗[X <: On#Objects, Y <: On#Objects] = 
        P# ×[X, Y]

      type I = 
        P# ∗
      // format: on
    }

  @inline
  final def monoidalCategory[P <: Product]
    : is[P] -> MonoidalCategory.is[CartesianMonoidalCategory[P]] =
    λ { product: is[P] =>
      new CartesianMonoidalCategoryImpl {

        type On =
          P#On

        // format: off
        type ⊗[X <: On#Objects, Y <: On#Objects] = 
          P# ×[X, Y]

        type I = 
          P# ∗
        // format: on

        val on =
          product.on

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

        def lunit[A <: On#Objects]: On#C[A, I ⊗ A] =
          Product(product) ⊢ { erase ^ id }

        def unitr[A <: On#Objects]: On#C[A ⊗ I, A] =
          product.left

        def runit[A <: On#Objects]: On#C[A, A ⊗ I] =
          Product(product) ⊢ { id ^ erase }
      }
    }

  sealed abstract class CartesianMonoidalCategoryImpl extends MonoidalCategory
}
