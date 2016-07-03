package ohnosequences.stuff


trait AnyKleisliCategory extends AnyCategory {

  type Monad <: AnyMonad { type On = Cat }
  val monad: Monad
  type Cat <: AnyCategory
  val cat: Cat

  type Objects = Cat#Objects
  type C[X <: Objects, Y <: Objects] = Cat#C[X, Monad#F[Y]]

  final def compose[X <: Objects, Y <: Objects, Z <: Objects](g: C[Y,Z], f: C[X,Y]): C[X,Z] = {

    // import AnyCategory._
    // import inUCat._

    val c = AnyCategory.is(cat)

    val z: Cat#C[X, Monad#F[Monad#F[Z]]] = c.compose(
      AnyMonad.is(monad)(g) , f
    )
    c.compose(
      AnyNaturalTransformation.is(AnyMonad.is(monad).μ).at[Z],
      z
    )
    // f.>=>(monad(g))(cat) //  >=> monad.μ.at
    // monad.μ.at ∘ monad(g) ∘ f
    ???
  }

  // type UCat <: AnyCategory
  // val uCat: AnyCategory.is[UCat]
  // type MFnctr <: AnyF { type Source = kc.UCat; type Target = kc.UCat }
  // lazy val mfnctr: AnyF.is[MFnctr] = mnd.fnctr
  //
  // type Mnd <: AnyM { type On = UCat; type Fnctr = MFnctr }
  // val mnd: AnyM.is[Mnd]
  //
  // // This is a recurring pattern when wrapping a type; can it be abstracted?
  // type Morphisms = AnyKleisliMorphismFor[UCat, MFnctr]
  // // wrap, unwrap
  // implicit def asKleisliMorphism[X <: Objects, Y <: Objects](f: UCat#C[X,MFnctr#F[Y]]):
  //   C[X,Y] = new C(f)
  // implicit def asUCatC[X <: Objects, Y <: Objects](ff: C[X,Y]): UCat#C[X,MFnctr#F[Y]] = ff.f
  //
  // type C[X <: UCat#Objects, Y <: UCat#Objects] = KleisliMorphism[UCat, MFnctr, X, Y]
  //
  // import AnyCategory._
  // lazy val inUCat = CategoryModule(uCat)
  //
  // def id[X <: Objects]: C[X,X] = mnd.η.at[X]
  //
  // final def compose[X <: Objects, Y <: Objects, Z <: Objects](g: C[Y,Z], f: C[X,Y]): C[X,Z] = {
  //
  //   import inUCat._
  //
  //   mnd.μ.at ∘ mnd(g) ∘ f
  // }
}
