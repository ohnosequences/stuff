package ohnosequences.stuff

abstract class DistributiveCategory { dist =>

  type Cat <: Category
  val cat: Category.is[Cat]

  type Products <: Product { type On = Cat }
  val products: Product.is[Products]

  type Coproducts <: Coproduct { type On = Cat }
  val coproducts: Coproduct.is[Coproducts]

  // format: off
  type ×[A <: Cat#Objects, B <: Cat#Objects] =
    Products# ×[A, B]

  type +[A <: Cat#Objects, B <: Cat#Objects] =
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

  type is[D <: DistributiveCategory] =
    D {
      type Cat        = D#Cat
      type Products   = D#Products
      type Coproducts = D#Coproducts
    }

  final class Syntax[Dist <: DistributiveCategory](val dist: is[Dist]) {

    // type aliases
    /////////////////////////////////////////////////////////////////////////
    @infix
    type >=>[X <: Dist#Cat#Objects, Y <: Dist#Cat#Objects] =
      Dist#Cat#C[X, Y]

    // format: off
    @infix
    type ×[X <: Dist#Cat#Objects, Y <: Dist#Cat#Objects] =
      dist.×[X, Y] // TODO I have no clue why I need `dist.` here

    type ∗ =
      Dist#Products# ∗

    @infix
    type +[X <: Dist#Cat#Objects, Y <: Dist#Cat#Objects] =
      dist.+[X, Y] // TODO I have no clue why I need `dist.` here

    type ∅ =
      Dist#Products# ∗
    // format: on

    // implicits
    /////////////////////////////////////////////////////////////////////////
    @inline
    implicit final val _on: Category.is[Dist#Cat] =
      dist.cat

    @inline
    implicit final val _prod: Product.is[Dist#Products] =
      dist.products.asInstanceOf[Product.is[Dist#Products]]

    @inline
    implicit final def categoryMorphismSyntax[
        X <: Dist#Cat#Objects,
        Y <: Dist#Cat#Objects
    ](f: X >=> Y): Category.MorphismSyntax[Dist#Cat, X, Y] =
      new Category.MorphismSyntax[Dist#Cat, X, Y](f)

    // what the hell
    type _P = Dist#Products { type On = Dist#Cat }

    @inline
    implicit final def productMorphismSyntax[X <: Dist#Cat#Objects,
                                             Y <: Dist#Cat#Objects](
        f: Dist#Cat#C[X, Y]): Product.ProductMorphismSyntax[_P, X, Y] =
      new Product.ProductMorphismSyntax[_P, X, Y](f.asInstanceOf[_P#On#C[X, Y]])
  }
}
