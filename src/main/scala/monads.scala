package ohnosequences.stuff

import NaturalTransformation._
import Functor.{∘}

abstract class Monad {

  type On <: Functor.Endo
  val on: Functor.is[On]

  type OnCat = On#Source

  // NOTE I need Functor.is due to the bound on composition
  val μ: (Functor.is[On] ∘ Functor.is[On]) ~> Functor.is[On]

  val ι: Functor.is[Functor.Identity[OnCat]] ~> Functor.is[On]
}

object Monad {

  type is[M <: Monad] =
    M {
      type On = M#On
    }

  type on[F0 <: Functor { type Target = Source }] =
    Monad { type On = F0 }

  type Identity[Cat <: Category] =
    IdentityImpl {

      type On =
        Functor.Identity[Cat]
    }

  @inline
  final def idMonad[Cat <: Category]: Category.is[Cat] -> is[Identity[Cat]] =
    Functor.identity >-> identity

  @inline
  final def identity[Cat <: Category]
    : Functor.is[Functor.Identity[Cat]] -> is[Identity[Cat]] =
    λ { idF =>
      new IdentityImpl {

        type On =
          Functor.Identity[Cat]

        val on: Functor.is[On] =
          idF

        val μ: (Functor.is[On] ∘ Functor.is[On]) ~> Functor.is[On] =
          new Between[Functor.is[On] ∘ Functor.is[On], Functor.is[On]](
            on >-> on,
            on
          ) {

            @inline
            final def apply[X <: SourceFunctor#Source#Objects]
              : SourceFunctor#Target#C[X, X] =
              on.source.identity
          }

        val ι: Functor.is[Functor.Identity[OnCat]] ~> Functor.is[On] =
          NaturalTransformation identity (Functor identity on.source)
      }
    }

  sealed abstract class IdentityImpl extends Monad
}
