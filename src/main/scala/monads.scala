package ohnosequences.stuff

trait AnyMonad extends AnyFunctor {

  type On <: AnyCategory
  val on: On

  type Source = On
  lazy val source = on
  type Target = On
  lazy val target = on

  type F[X <: On#Objects] = Functor#F[X]

  type Functor <: On ⟶ On
  val functor: Functor

  type η <: AnyIdentityFunctor ~> Functor
  val η: η

  type μ <: (Functor >=> Functor) ~> Functor
  val μ: μ

  final def apply[X <: On#Objects, Y <: On#Objects](f: On#C[X,Y]): On#C[F[X],F[Y]] =
    AnyFunctor.is(functor)(f)
}

case class IdentityMonad[C <: AnyCategory](val on: C) extends AnyMonad {

  type On = C

  type Functor = IdentityFunctor[On]
  val functor = on.Id

  type η  = IdentityNaturalTransformation[Functor]
  val η   = IdentityNaturalTransformation(functor)

  case object Mu extends AnyNaturalTransformation {

    type SourceF = (Functor >=> Functor)
    val sourceF = functor >=> functor
    type TargetF = Functor
    val targetF = functor

    def at[X <: On#Objects]: On#C[SourceF#F[X], TargetF#F[X]] =
      AnyCategory.is(on).id[X]
  }

  type μ  = Mu.type
  val μ   = Mu
}
