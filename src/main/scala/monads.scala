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

  type F[X <: On#Objects] = Functor#F[X]

  type η <: AnyNaturalTransformation {
    type SourceF = AnyFunctor.is[IdentityFunctor[On]]; // type SourceCat = On;
    type TargetF = Functor //<: Functor
  }
    // AnyIdentityFunctor ~> Functor {  }
  val η: AnyNaturalTransformation.is[η]

  type μ <: (Functor >=> Functor) ~> Functor
  val μ: AnyNaturalTransformation.is[μ]

  def apply[X <: On#Objects, Y <: On#Objects](f: On#C[X,Y]): On#C[F[X], F[Y]] = AnyFunctor.is(functor)(f)
}

case object AnyMonad {

  def is[M <: AnyMonad](m: M): is[M] = m.asInstanceOf[is[M]]

  type is[M <: AnyMonad] = M with AnyMonad {

    type On = M#On
    type Source = M#Functor#Source
    type Target = M#Functor#Target
    type Functor = M#Functor
    type η = M#η
    type μ = M#μ
  }
}

case class IdentityMonad[C <: AnyCategory](val id: IdentityFunctor[C]) extends AnyMonad {

  type On = C

  type Functor = AnyFunctor.is[IdentityFunctor[On]]
  val functor = AnyFunctor.is(id)

  type η  = IdentityNaturalTransformation[IdentityFunctor[On]]
  val η   = IdentityNaturalTransformation(id)

  case object Mu extends AnyNaturalTransformation {

    type SourceF = (Functor >=> Functor)
    lazy val sourceF = functor >=> functor
    type TargetF = Functor
    lazy val targetF = functor

    def at[X <: On#Objects]: On#C[SourceF#F[X], TargetF#F[X]] =
      AnyCategory.is(on).id[X]
  }

  type μ  = Mu.type
  lazy val μ   = Mu
}
