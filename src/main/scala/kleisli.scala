package ohnosequences.stuff


trait AnyKleisliCategory extends AnyCategory { kleisli =>

  type Cat <: AnyCategory
  lazy val cat: Cat = monad.on

  type Functor <: Cat ⟶ Cat
  type Monad <: AnyMonad {
    type On = kleisli.Cat;
    type Functor = kleisli.Functor
  }
  val monad: Monad

  type Objects = Cat#Objects

  type C[X <: Objects, Y <: Objects] = Cat#C[X, Monad#F[Y]]

  final def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] = (g,f) => {

    // TODO proper syntax
    import AnyCategory._
    implicit val c = AnyCategory.is(monad.on)
    val m = AnyMonad.is(monad)
    val mu = AnyNaturalTransformation.is[Monad#μ](monad.μ)

    (f:Cat#C[X,Monad#F[Y]]) >=> m(g) >=> mu.at
  }

  final def id[X <: Objects]: C[X,X] = AnyMonad.is(monad).η[X]
}

case class KleisliCategory[
  On0 <: AnyCategory,
  Functor0 <: AnyFunctor { type Source = On0; type Target = On0 },
  Monad0 <: AnyMonad { type On = On0; type Functor = Functor0 }
]
(val monad: Monad0) extends AnyKleisliCategory {

  type Cat = On0
  type Functor = Functor0
  type Monad = Monad0
}
//
// trait AnyKleisliFunctor extends AnyFunctor {
//
//   type Source = target.Cat
//   lazy val source: Source = target.cat
//   type Target <: AnyKleisliCategory
//   val target: Target
//
//   type F[X <: target.Cat#Objects] = X
//
//   def apply[A <: target.Cat#Objects, B <: Source#Objects](f: target.Cat#C[A,B]): Target#C[F[A],F[B]] = {
//
//     // TODO proper syntax
//     import AnyCategory._
//     implicit val c = AnyCategory.is(target.cat)
//
//     f >=> target.monad.η[B]
//   }
// }
//
// case class KleisliFunctor[KC <: AnyKleisliCategory](val target: KC) extends AnyKleisliFunctor {
//
//   type Target = KC
// }
//
// trait AnyKleisliForget extends AnyFunctor { forget =>
//
//   type Monad <: AnyMonad
//   val monad: Monad
//
//   type Source <: AnyKleisliCategory {
//     type Monad = forget.Monad;
//     type Cat = forget.monad.On
//     type Objects = forget.monad.On#Objects
//     type C[X <: Objects, Y <: Objects] = Cat#C[X, forget.monad.Functor#F[Y]]
//   }
//   val source: Source // = KleisliCategory[Monad](monad).asInstanceOf[Source]
//
//   type Target = monad.On
//   lazy val target = monad.on
//
//   type F[X <: Source#Objects] = monad.F[X]
//
//   override def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X], F[Y]] = {
//
//     import AnyCategory._
//     implicit val c = AnyCategory.is(monad.on)
//
//     monad(f: monad.On#C[X, monad.F[Y]]) >=> monad.μ[Y]
//   }
// }
//
// case class KleisliForget[M <: AnyMonad](val s: KleisliCategory[M]) extends AnyKleisliForget {
//
//   kf =>
//
//   type Monad = M
//   lazy val monad: Monad = source.monad
//
//   type Source = KleisliCategory[M] { type Monad = kf.Monad;
//   type Cat = kf.monad.On
//   type Objects = kf.monad.On#Objects
//   type C[X <: Objects, Y <: Objects] = Cat#C[X, kf.monad.Functor#F[Y]]
//   }
//
//   val source: Source = s.asInstanceOf[Source]
// }
