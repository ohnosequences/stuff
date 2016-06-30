package ohnosequences.stuff


  trait AnyFunctor { functor =>

    type Source <: AnyCategory
    val source: AnyCategory.is[Source]

    type Target <: AnyCategory
    val target: AnyCategory.is[Target]

    type F[Z <: Source#Objects] <: Target#Objects

    // def map[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X], F[Y]]

    def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X], F[Y]]
  }

  abstract class Functor[
    Source0 <: AnyCategory,
    Target0 <: AnyCategory
  ]
  (
    val source: AnyCategory.is[Source0],
    val target: AnyCategory.is[Target0]
  )
    extends
      AnyFunctor
  {
    type Source = Source0
    type Target = Target0
  }

  case object AnyFunctor {

    type between[src <: AnyCategory, tgt <: AnyCategory] = AnyFunctor {

      type Source = src;
      type Target = tgt;
    }

    type ⟶[src <: AnyCategory, tgt <: AnyCategory] = between[src,tgt]
    type ➔[src <: AnyCategory, tgt <: AnyCategory] = between[src,tgt]

    type is[functor <: AnyFunctor] = functor with AnyFunctor {

      type Source = functor#Source;
      type Target = functor#Target;
      type F[Z <: Source#Objects] = functor#F[Z]
    }

    type to[src <: AnyCategory] = AnyFunctor { type Source = src; }
    type from[tgt <: AnyCategory] = AnyFunctor { type Target = tgt; }

    type ∘[g0 <: AnyFunctor { type Source = f0#Target }, f0 <: AnyFunctor] =
      FunctorComposition[AnyFunctor.is[f0],AnyFunctor.is[g0]]

    type >=>[f0 <: AnyFunctor, g0 <: AnyFunctor { type Source = f0#Target }] =
      FunctorComposition[f0,g0]

    implicit final class FunctorSyntax[F0 <: AnyFunctor](val f: F0) extends AnyVal {

      def >=>[G0 <: AnyFunctor { type Source = F0#Target }](g: G0): F0 >=> G0 = new FunctorComposition[F0,G0](f,g)
    }
  }

  trait AnyIdentityFunctor extends AnyFunctor { idf =>

    type On <: AnyCategory
    val on: AnyCategory.is[On]

    type Source = On
    lazy val source = on
    type Target = On
    lazy val target = on

    type F[Z <: Source#Objects] = Z

    final def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X], F[Y]] = f
  }

  class IdentityFunctor[On0 <: AnyCategory](val on: AnyCategory.is[On0])
    extends
      AnyIdentityFunctor
  {
    type On = On0
  }

  trait AnyFunctorComposition extends AnyFunctor { composition =>
    //
    // type SourceC <: AnyCategory
    // type MiddleC <: AnyCategory
    // type TargetC <: AnyCategory

    type FirstF <: AnyFunctor
    val firstF: FirstF

    type SecondF <: AnyFunctor {
        type Source = FirstF#Target;
      }
    val secondF: SecondF

    type Source = firstF.Source
    lazy val source: AnyCategory.is[Source] = firstF.source
    type Target = secondF.Target
    lazy val target: AnyCategory.is[Target] = secondF.target

    type F[Z <: Source#Objects] = secondF.F[firstF.F[Z]]

    final def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y])
      : Target#C[F[X], F[Y]] = secondF(firstF(f))
  }

  class FunctorComposition[
    F0 <: AnyFunctor,
    S0 <: AnyFunctor { type Source = F0#Target }
  ]
  (val firstF:F0, val secondF: S0) extends AnyFunctorComposition {

    // type SourceC = firstF.Source with F0#Source
    // type MiddleC = firstF.Target
    // type TargetC = secondF.Target

    // would be nice if I could avoid this here
    type FirstF = F0
    type SecondF = S0
  }
