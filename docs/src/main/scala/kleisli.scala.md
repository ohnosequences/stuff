
```scala
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

    implicit val c = AnyCategory.is(monad.on)

    val m = AnyMonad.is(monad)
    val mu = AnyNaturalTransformation.is[Monad#μ](monad.μ)

    (f:Cat#C[X,Monad#F[Y]]) >=> m(g) >=> mu.at
  }

  final def id[X <: Objects]: C[X,X] = AnyMonad.is(monad).η[X]
}

case object AnyKleisliCategory {

  implicit final class KleisliCategorySyntax[KlC <: AnyKleisliCategory](val klC: KlC) extends AnyVal {

    def freeF[
      KlC0 >: KlC           <: KlC { type Cat = C0; type Functor = F0; type Monad = M0 },
      C0    >: KlC#Cat      <: AnyCategory,
      F0    >: KlC#Functor  <: AnyFunctor { type Source = C0; type Target = C0 },
      M0    >: KlC#Monad    <: AnyMonad { type On = C0; type Functor = F0 }

    ]
    : KleisliFunctor[C0,F0,M0,KlC0] =
      KleisliFunctor(klC: KlC0)

    def forgetfulF[
      KlC0 >: KlC           <: KlC { type Cat = C0; type Functor = F0; type Monad = M0 },
      C0    >: KlC#Cat      <: AnyCategory,
      F0    >: KlC#Functor  <: AnyFunctor { type Source = C0; type Target = C0 },
      M0    >: KlC#Monad    <: AnyMonad { type On = C0; type Functor = F0 }
    ]
    : KleisliForget[C0,F0,M0,KlC0] =
      KleisliForget(klC: KlC0)
  }
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

trait AnyKleisliFunctor extends AnyFunctor { kleisliF =>

  type On <: AnyCategory

  type Source = On
  lazy val source: Source = target.cat

  type Monad <: AnyMonad { type On = kleisliF.On; type Functor = kleisliF.Functor }
  type Functor <: AnyFunctor { type Source = kleisliF.On; type Target = kleisliF.On }

  type Target <: AnyKleisliCategory {
      type Cat = kleisliF.On;
      type Monad = kleisliF.Monad
      type Functor = kleisliF.Functor
    }
  val target: Target

  type F[X <: Source#Objects] = X

  override def apply[A <: Source#Objects, B <: Source#Objects](f: Source#C[A,B]): On#C[A,Functor#F[B]] = {

    implicit val c = AnyCategory.is(target.cat)

    val eta = AnyNaturalTransformation.is[Monad#η](target.monad.η)

    f >=> eta.at[B]
  }
}

case class KleisliFunctor[
  On0 <: AnyCategory,
  F0 <: AnyFunctor { type Source = On0; type Target = On0 },
  M0 <: AnyMonad { type On = On0; type Functor = F0 },
  KC <: AnyKleisliCategory { type Cat = On0; type Monad = M0; type Functor = F0 }
](val target: KC) extends AnyKleisliFunctor {

  type On = On0
  type Functor = F0
  type Monad = M0
  type Target = KC
}

trait AnyKleisliForget extends AnyFunctor { forget =>

  type C <: AnyCategory
  type Functor <: AnyFunctor { type Source = C; type Target = C }

  type Monad <: AnyMonad { type On = C; type Functor = forget.Functor }
  lazy val monad: Monad = source.monad

  type Source <: AnyKleisliCategory {
    type Monad = forget.Monad;
    type Cat = forget.Monad#On
    type Functor = forget.Functor
  }
  val source: Source

  type Target = Monad#On
  lazy val target = monad.on

  type F[X <: Source#Objects] = Monad#Functor#F[X]

  override def apply[X <: Source#Objects, Y <: Source#Objects](f: C#C[X,F[Y]]): Target#C[F[X], F[Y]] = {

    implicit val c = AnyCategory.is[Monad#On](monad.on)

    AnyMonad.is(monad)(f: Monad#On#C[X, Monad#Functor#F[Y]]) >=> AnyNaturalTransformation.is[Monad#μ](monad.μ)[Y]
  }
}

case class KleisliForget[
  C0 <: AnyCategory,
  F0 <: AnyFunctor { type Source = C0; type Target = C0 },
  M <: AnyMonad { type On = C0; type Functor = F0 },
  KC <: AnyKleisliCategory { type Cat = C0; type Monad = M; type Functor = F0 }
]
(val s: KC) extends AnyKleisliForget {

  type C = C0
  type Functor = F0
  type Monad = M

  type Source = KC
  val source: Source = s
}

```




[test/scala/categories.scala]: ../../test/scala/categories.scala.md
[main/scala/monoidalCategories.scala]: monoidalCategories.scala.md
[main/scala/distributiveLaws.scala]: distributiveLaws.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/monads.scala]: monads.scala.md
[main/scala/monoidalFunctors.scala]: monoidalFunctors.scala.md
[main/scala/functors.scala]: functors.scala.md
[main/scala/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/kleisli.scala]: kleisli.scala.md
[main/scala/categories.scala]: categories.scala.md