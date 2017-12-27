package ohnosequences.stuff

abstract class DistributiveCategory { dist =>

  type Cat <: Category
  val cat: Category.is[Cat]

  type Products <: Product { type On          = Cat }
  val products: Product.is[Products { type On = Cat }]

  type Coproducts <: Coproduct { type On            = Cat }
  val coproducts: Coproduct.is[Coproducts { type On = Cat }]

  // format: off
  final type ×[A <: Cat#Objects, B <: Cat#Objects] =
    Products# ×[A, B]

  final type +[A <: Cat#Objects, B <: Cat#Objects] =
    Coproducts# +[A, B]
  // format: on

  final def pack[
      A <: Cat#Objects,
      X <: Cat#Objects,
      Y <: Cat#Objects,
  ]: Cat#C[(A × X) + (A × Y), A × (X + Y)] =
    Product(products) ⊢ {
      Coproduct(coproducts) ⊢ {
        (id × coproducts.left[X, Y]) | (id × coproducts.right[X, Y])
      }
    }

  def expand[
      A <: Cat#Objects,
      X <: Cat#Objects,
      Y <: Cat#Objects,
  ]: Cat#C[A × (X + Y), (A × X) + (A × Y)]
}

object DistributiveCategory {

  type ProductsOn[C0 <: Category] =
    Product { type On = C0 }

  // format: off
  type is[D <: DistributiveCategory] =
    D {
      type Cat        = D#Cat
      type Products   = D#Products
      type Coproducts = D#Coproducts
    }

  protected type reallyIs[D <: DistributiveCategory] =
    D {
      type Cat        = D#Cat
      type Products   = D#Products { type On = Cat }
      type Coproducts = D#Coproducts { type On = Cat }
    }

  final class Syntax[Dist <: DistributiveCategory](val dist: is[Dist]) {

    // type aliases
    /////////////////////////////////////////////////////////////////////////
    type Cat =
      Dist#Cat

    type Objects =
      Cat#Objects

    @infix
    type >=>[X <: Objects, Y <: Objects] =
      Cat#C[X, Y]

    // what the hell
    private type _P = Dist#Products { type On   = Dist#Cat }
    private type _C = Dist#Coproducts { type On = Dist#Cat }

    // format: off
    @infix
    type ×[X <: Objects, Y <: Objects] =
      reallyIs[Dist] # Products # ×[X, Y]

    type ∗ =
      reallyIs[Dist] # Products # ∗

    @infix
    type +[X <: Objects, Y <: Objects] =
      reallyIs[Dist] # Coproducts # +[X, Y]

    type ∅ =
      reallyIs[Dist] # Coproducts # ∅
    // format: on

    // function aliases
    /////////////////////////////////////////////////////////////////////////
    @inline
    final def id[X <: Objects]: X >=> X =
      dist.cat.identity

    @inline
    final def outLeft[A <: Objects, B <: Objects]: (A × B) >=> A =
      dist.products.left

    @inline
    final def outRight[A <: Objects, B <: Objects]: (A × B) >=> B =
      dist.products.right

    @inline
    final def erase[A <: Objects]: A >=> ∗ =
      dist.products.erase[A]

    @inline
    final def duplicate[Z <: Objects]: Z >=> (Z × Z) =
      id ^ id

    @inline
    final def Δ[Z <: Objects]: Z >=> (Z × Z) =
      duplicate

    // coproducts
    @inline
    final def inLeft[A <: Objects, B <: Objects]: A >=> (A + B) =
      dist.coproducts.left

    @inline
    final def inRight[A <: Objects, B <: Objects]: B >=> (A + B) =
      dist.coproducts.right

    @inline
    final def intro[A <: Objects]: ∅ >=> A =
      dist.coproducts.intro

    @inline
    final def any[Z <: Objects]: (Z + Z) >=> Z =
      id | id

    @inline
    final def ∇[Z <: Objects]: (Z + Z) >=> Z =
      any

    // dist
    @inline
    final def expand[
        A <: Objects,
        X <: Objects,
        Y <: Objects,
    ]: (A × (X + Y)) >=> ((A × X) + (A × Y)) =
      dist
        .asInstanceOf[reallyIs[Dist]]
        .expand

    @inline
    final def pack[
        A <: Objects,
        X <: Objects,
        Y <: Objects,
    ]: ((A × X) + (A × Y)) >=> (A × (X + Y)) =
      dist
        .asInstanceOf[reallyIs[Dist]]
        .pack

    // implicits
    /////////////////////////////////////////////////////////////////////////
    @inline
    implicit final val _cat: Category.is[Cat] =
      dist.cat

    @inline
    implicit final val _prod: Product.is[_P] =
      dist.products

    @inline
    implicit final val _coprod: Coproduct.is[_C] =
      dist.coproducts

    @inline
    implicit final def categoryMorphismSyntax[
        X <: Objects,
        Y <: Objects
    ](f: X >=> Y): Category.MorphismSyntax[Cat, X, Y] =
      new Category.MorphismSyntax(f)

    @inline
    implicit final def productMorphismSyntax[
        X <: Objects,
        Y <: Objects
    ](f: X >=> Y): Product.ProductMorphismSyntax[_P, X, Y] =
      new Product.ProductMorphismSyntax[_P, X, Y](f)

    @inline
    implicit final def coproductMorphismSyntax[
        X <: Objects,
        Y <: Objects
    ](f: X >=> Y): Coproduct.SumMorphismSyntax[_C, X, Y] =
      new Coproduct.SumMorphismSyntax[_C, X, Y](f)
  }
}
