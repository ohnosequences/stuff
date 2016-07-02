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

  def is[F <: AnyFunctor](f: F): AnyFunctor.is[F] = f.asInstanceOf[AnyFunctor.is[F]]

  type is[functor <: AnyFunctor] = functor with AnyFunctor {

    type Source = functor#Source;
    type Target = functor#Target;
    type F[Z <: Source#Objects] = functor#F[Z]
  }
//
//   type ⟶[src <: AnyCategory, tgt <: AnyCategory] = AnyFunctor {
//
//     type Source = src;
//     type Target = tgt;
//   }
//   //
//   // type ∘[g0 <: AnyFunctor { type Source = f0#Target }, f0 <: AnyFunctor] =
//   //   FunctorComposition[f0,g0]
//   //
  type >=>[f0 <: AnyFunctor, g0 <: AnyFunctor { type Source = f0#Target }] =
    FunctorComposition[f0,g0]

  implicit final class FunctorSyntax[F0 <: AnyFunctor](val f: F0) extends AnyVal {

    def id: IdentityNaturalTransformation[F0] = IdentityNaturalTransformation(f)

    def >=>[G0 <: AnyFunctor { type Source = F0#Target }](g: G0): F0 >=> G0 = new FunctorComposition[F0,G0](f,g)
  }
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

class IdentityFunctor[On0 <: AnyCategory](val on: On0) extends AnyIdentityFunctor {

  type On = On0
}

trait AnyFunctorComposition extends AnyFunctor {

  type FirstF <: AnyFunctor
  val firstF: FirstF

  type SecondF <: AnyFunctor { type Source = FirstF#Target }
  val secondF: SecondF

  type Source = FirstF#Source
  lazy val source: Source = firstF.source
  type Target = SecondF#Target
  lazy val target: Target = secondF.target

  type F[Z <: Source#Objects] = SecondF#F[FirstF#F[Z]]
}

class FunctorComposition[
  F0 <: AnyFunctor,
  S0 <: AnyFunctor { type Source = F0#Target }
]
(
  val firstF: F0,
  val secondF: S0
)
extends AnyFunctorComposition {

  type FirstF = F0
  type SecondF = S0

  // NOTE I know this is dangerous. I want to check whether everything works as long as you use it safely
  final def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X], F[Y]] =
    AnyFunctor.is(secondF)(AnyFunctor.is(firstF)(f))
}
