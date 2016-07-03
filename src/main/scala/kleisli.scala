package ohnosequences.stuff


trait AnyKleisliCategory extends AnyCategory {

  type Monad <: AnyMonad
  val monad: Monad
  type Cat = monad.On
  val cat: AnyCategory.is[Cat] = monad.on

  type M[X <: Objects] = monad.Functor#F[X]

  type Objects = Cat#Objects
  type C[X <: Objects, Y <: Objects] = Cat#C[X, monad.Functor#F[Y]]

  final def compose[X <: Objects, Y <: Objects, Z <: Objects](g: C[Y,Z], f: C[X,Y]): C[X,Z] = {

    // TODO proper syntax
    import AnyCategory._
    implicit val c = cat

    (f:Cat#C[X,M[Y]]) >=> monad.functor(g) >=> monad.μ.at
  }
}
