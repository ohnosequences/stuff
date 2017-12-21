package ohnosequences.stuff

abstract class DistributiveCategory {

  type Cat <: Category
  val cat: Category.is[Cat]

  type Prods <: Product { type On = Cat }
  val prods: Product.is[Prods]

  type Coprods <: Coproduct { type On = Cat }
  val coprods: Coproduct.is[Coprods]

  // format: off
  type ×[A <: Cat#Objects, B <: Cat#Objects] =
    Prods# ×[A, B]

  type +[A <: Cat#Objects, B <: Cat#Objects] =
    Coprods# +[A, B]
  // format: on

  final def pack[
      A <: Cat#Objects,
      X <: Cat#Objects,
      Y <: Cat#Objects,
  ]: Cat#C[(A × X) + (A × Y), A × (X + Y)] =
    Product(prods) ⊢ {
      Coproduct(coprods) ⊢ {
        (id × coprods.left[X, Y]) | (id × coprods.right[X, Y])
      }
    }

  def expand[
      A <: Cat#Objects,
      X <: Cat#Objects,
      Y <: Cat#Objects,
  ]: Cat#C[A × (X + Y), (A × X) + (A × Y)]
}
