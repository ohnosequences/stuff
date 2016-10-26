package ohnosequences.stuff

case object syntax {

  /*
    ## Categories
  */
  implicit final def morphismsSyntax[
    Cat <: AnyCategory,
    Y <: Cat#Objects,
    Z <: Cat#Objects
  ](g: Cat#C[Y,Z])(implicit cat: Cat)
  : MorphismsSyntax[Cat,Y,Z] =
    MorphismsSyntax(g)

  implicit final def categorySyntax[Cat <: AnyCategory](cat: Cat): CategorySyntax[Cat] =
    CategorySyntax(cat)

  implicit def functorSyntax[F0 <: AnyFunctor](f0: F0): FunctorSyntax[F0] =
    FunctorSyntax(f0)
}
