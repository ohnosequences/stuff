package ohnosequences.stuff

trait AnyFunctor {

  type Source <: AnyCategory
  val source: Source

  type Target <: AnyCategory
  val target: Target

  type F[Z <: Source#Objects] <: Target#Objects

  def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X], F[Y]]
}

abstract class Functor[
  Source0 <: AnyCategory,
  Target0 <: AnyCategory
]
(
  val source: Source0,
  val target: Target0
)
extends AnyFunctor {

  type Source = Source0
  type Target = Target0
}

case object AnyFunctor {

  def is[functor <: AnyFunctor](f: functor): AnyFunctor.is[functor] =
    f.asInstanceOf[AnyFunctor.is[functor]]

  type is[functor <: AnyFunctor] = functor {

    type Source = functor#Source;
    type Target = functor#Target;
    type F[Z <: Source#Objects] = functor#F[Z]
  }
}

final case class FunctorSyntax[functor <: AnyFunctor](val functor: functor) extends AnyVal {

  def >=>[G0 <: AnyFunctor { type Source = functor#Target }](g: G0): functor >=> G0 =
    FunctorComposition(functor,g)

  def id[
    F0  >: functor         <: functor { type Source = C; type Target = D },
    C   >: functor#Source  <: functor#Source,
    D   >: functor#Target  <: functor#Target
  ]
  : IdentityNaturalTransformation[C, F0, D] =
    IdentityNaturalTransformation(functor: F0)
}

trait AnyIdentityFunctor extends AnyFunctor {

  type On <: AnyCategory
  val on: On

  type Source = On
  lazy val source: Source = on

  type Target = On
  lazy val target: Target = on

  type F[Z <: Source#Objects] = Z

  final def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X], F[Y]] = f
}

case class IdentityFunctor[On0 <: AnyCategory](val on: On0) extends AnyIdentityFunctor {

  type On = On0
}

trait AnyFunctorComposition extends AnyFunctor {

  type First <: AnyFunctor
  val first: First

  type Second <: AnyFunctor { type Source = First#Target }
  val second: Second

  type Source = First#Source
  lazy val source: Source = first.source
  type Target = Second#Target
  lazy val target: Target = second.target

  type F[Z <: Source#Objects] = Second#F[First#F[Z]]
}

case class FunctorComposition[
  F0 <: AnyFunctor,
  S0 <: AnyFunctor { type Source = F0#Target }
]
(
  val first: F0,
  val second: S0
)
extends AnyFunctorComposition {

  type First = F0
  type Second = S0

  final def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X], F[Y]] =
    AnyFunctor.is(second)(AnyFunctor.is(first)(f))
}
