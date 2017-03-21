package ohnosequences.stuff

import ohnosequences.stuff.syntax._

trait AnyMonad {

  type On <: AnyCategory
  lazy val on: On = functor.source

  type Source = On
  lazy val source: Source = on
  type Target = On
  lazy val target: Target = on

  type Functor <: On ⟶ On
  val functor: Functor

  type F[X <: Source#Objects] = Functor#F[X]

  type η <: AnyNaturalTransformation {
    type SourceCategory = On
    type TargetCategory = On
    type SourceFunctor = IdentityFunctor[On]
    type TargetFunctor = Functor
  }
  val η: η

  type μ <: AnyNaturalTransformation {

    type SourceCategory = On
    type TargetCategory = On

    type SourceFunctor = Functor >-> Functor
    type TargetFunctor = Functor
  }
  val μ: μ

  def apply[X <: On#Objects, Y <: On#Objects](f: On#C[X,Y]): On#C[F[X], F[Y]] =
    AnyFunctor.is(functor)(f)
}

abstract class MonadOn[
  On0 <: AnyCategory,
  Functor0 <: On0 ⟶ On0
](
  val cat: On0
)(
  val functor: Functor0
)
extends AnyMonad {

  type On = On0
  type Functor = Functor0
}

case object AnyMonad {

  def is[M <: AnyMonad](m: M): is[M] =
    m.asInstanceOf[is[M]]

  type is[M <: AnyMonad] = M {

    type On = AnyCategory.is[M#On]
    type Source = M#Source
    type Target = M#Target
    type Functor = AnyFunctor.is[M#Functor]
    type η = M#η
    type μ = M#μ
  }

  implicit class MonadSyntax[monad <: AnyMonad](val monad: monad) extends scala.AnyVal {

    def kleisliCategory[
      M0 >: monad         <: monad { type On = C; type Functor = F0 },
      F0 >: monad#Functor <: monad#Functor { type Source = C; type Target = C },
      C  >: monad#On      <: monad#On
    ]
    : KleisliCategory[C, F0, M0] =
      KleisliCategory(monad: M0)
  }
}

case class IdentityMonad[Category <: AnyCategory](category: Category) extends MonadOn(category)(category.Id) {

  type η  = IdentityNaturalTransformation[On, IdentityFunctor[On], On]
  val η   = IdentityNaturalTransformation[On, IdentityFunctor[On], On](functor)

  case object Mu extends NaturalTransformation(on, functor >-> functor, functor, on) {

    def at[X <: On#Objects]: On#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
      AnyCategory.is(on).id[X]
  }

  type μ      = Mu.type
  lazy val μ  = Mu
}
