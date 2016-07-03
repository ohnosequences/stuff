package ohnosequences.stuff

trait AnyMonad extends AnyFunctor {

  type On <: AnyCategory
  val on: On

  type Source = On
  lazy val source = on
  type Target = On
  lazy val target = on

  type F[X <: Functor#Source#Objects] = Functor#F[X]

  type Functor <: On ⟶ On
  val functor: Functor

  type η <: AnyIdentityFunctor ~> Functor
  val η: η

  type μ <: (Functor >=> Functor) ~> Functor
  val μ: μ

  final def apply[X <: On#Objects, Y <: On#Objects](f: On#C[X,Y]): On#C[F[X],F[Y]] =
    AnyFunctor.is(functor)(f)
}

case object AnyMonad {

  def is[M <: AnyMonad](m: M): is[M] = m.asInstanceOf[is[M]]

  type is[M <: AnyMonad] = M with AnyMonad {

    type On = AnyCategory.is[M#On]
    // type Source = M#Source
    // type Target = M#Target
    type Functor = AnyFunctor.is[M#Functor]
// type Fnctr = AnyF.is[m0#Fnctr];
    type F[X <: M#Functor#Source#Objects] = M#Functor#F[X]

    type η = M#η
    type μ = M#μ
  }
}

// case class IdentityMonad[C <: AnyCategory](val on: C) extends AnyMonad {
//
//   type On = C
//
//   type Functor = IdentityFunctor[On]
//   val functor = on.Id
//
//   type η  = IdentityNaturalTransformation[Functor]
//   val η   = IdentityNaturalTransformation(functor)
//
//   case object Mu extends AnyNaturalTransformation {
//
//     type SourceF = (Functor >=> Functor)
//     val sourceF = functor >=> functor
//     type TargetF = Functor
//     val targetF = functor
//
//     def at[X <: On#Objects]: On#C[SourceF#F[X], TargetF#F[X]] =
//       AnyCategory.is(on).id[X]
//   }
//
//   type μ  = Mu.type
//   val μ   = Mu
// }
