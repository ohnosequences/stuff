package ohnosequences.stuff

abstract class DistributiveCategory {

  type Products <: Product
  val products: Product.is[Products]

  type Coproducts <: Coproduct { type On = Products#On }
  val coproducts: Coproduct.is[Coproducts]

  // format: off
  type ×[A <: Products#On#Objects, B <: Products#On#Objects] =
    Products# ×[A, B]

  type +[A <: Coproducts#On#Objects, B <: Coproducts#On#Objects] =
    Coproducts# +[A, B]
  // format: on

  final def pack[
      A <: Products#On#Objects,
      X <: Products#On#Objects,
      Y <: Products#On#Objects,
  ]: Products#On#C[(A × X) + (A × Y), A × (X + Y)] =
    Product(products) ⊢ {
      Coproduct(coproducts) ⊢ {
        (id × coproducts.left[X, Y]) | (id × coproducts.right[X, Y])
      }
    }

  def expand[
      A <: Products#On#Objects,
      X <: Products#On#Objects,
      Y <: Products#On#Objects,
  ]: Products#On#C[A × (X + Y), (A × X) + (A × Y)]
}

object DistributiveCategory {

  type is[D <: DistributiveCategory] =
    D {
      type Products   = D#Products
      type Coproducts = D#Coproducts { type On = D#Products#On }
    }

  final class Syntax[Dist <: DistributiveCategory](val dist: is[Dist]) {

    // type aliases
    /////////////////////////////////////////////////////////////////////////
    @infix
    type >=>[X <: Dist#Products#On#Objects, Y <: Dist#Products#On#Objects] =
      Dist#Products#On#C[X, Y]

    // format: off
    @infix
    type ×[X <: Dist#Products#On#Objects, Y <: Dist#Products#On#Objects] =
      Dist#Products# ×[X, Y]

    type ∗ =
      Dist#Products# ∗

    @infix
    type +[X <: Dist#Coproducts#On#Objects, Y <: Dist#Coproducts#On#Objects] =
      is[Dist]#Coproducts# +[X, Y]

    type ∅ =
      Dist#Products# ∗
    // format: on

    // implicits
    /////////////////////////////////////////////////////////////////////////
    @inline
    implicit final val _on: Category.is[Dist#Products#On] =
      dist.products.on

    @inline
    implicit final val _prod: Product.is[Dist#Products] =
      dist.products

    @inline
    implicit final val _coprod
      : Coproduct.is[Dist#Coproducts { type On = Dist#Products#On }] =
      dist.coproducts

    @inline
    implicit final def categoryMorphismSyntax[
        X <: Dist#Products#On#Objects,
        Y <: Dist#Products#On#Objects
    ](f: X >=> Y): Category.MorphismSyntax[Dist#Products#On, X, Y] =
      new Category.MorphismSyntax[Dist#Products#On, X, Y](f)

    @inline
    implicit final def productMorphismSyntax[
        X <: Dist#Products#On#Objects,
        Y <: Dist#Products#On#Objects
    ](f: Dist#Products#On#C[X, Y])
      : Product.ProductMorphismSyntax[Dist#Products, X, Y] =
      new Product.ProductMorphismSyntax(f)

    @inline
    implicit final def coproductMorphismSyntax[
        X <: Dist#Products#On#Objects,
        Y <: Dist#Products#On#Objects
    ](f: Dist#Products#On#C[X, Y]): Coproduct.SumMorphismSyntax[
      Dist#Coproducts { type On = Dist#Products#On },
      X,
      Y
    ] =
      new Coproduct.SumMorphismSyntax[
        Dist#Coproducts { type On = Dist#Products#On },
        X,
        Y
      ](f)
  }
}
