package ohnosequences.stuff

trait AnyMonad extends AnyFunctor {

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
    type SourceCat = On
    type TargetCat = On
    type SourceF = IdentityFunctor[On]
    type TargetF = Functor
  }
  val η: η

  type μ <: AnyNaturalTransformation {

    type SourceCat = On
    type TargetCat = On

    type SourceF = Functor >=> Functor
    type TargetF = Functor
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
    type Functor = AnyFunctor.is[M#Functor]
    type η = M#η
    type μ = M#μ
  }

  implicit class MonadSyntax[M <: AnyMonad](val m: M) extends AnyVal {

    def kleisliCategory[
      M0 >: M         <: M { type On = C; type Functor = F0 },
      F0 >: M#Functor <: M#Functor { type Source = C; type Target = C },
      C  >: M#On      <: M#On
    ]: KleisliCategory[C, F0, M0] = KleisliCategory(m: M0)
  }
}

case class IdentityMonad[C <: AnyCategory](c: C) extends MonadOn(c)(c.Id) {

  type η  = IdentityNaturalTransformation[On, IdentityFunctor[On], On]
  val η   = IdentityNaturalTransformation[On, IdentityFunctor[On], On](functor)

  case object Mu extends NaturalTransformation(on, functor >=> functor, functor, on) {

    def at[X <: On#Objects]: On#C[SourceF#F[X], TargetF#F[X]] =
      AnyCategory.is(on).id[X]
  }

  type μ      = Mu.type
  lazy val μ  = Mu
}
