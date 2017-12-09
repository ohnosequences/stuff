package ohnosequences.stuff

import NaturalTransformation._
import Functor.{∘}

abstract class Monad {

  type On <: Functor.Endo
  val on: Functor.is[On]

  type OnCat = On#Source

  // NOTE I need Functor.is due to the bound on composition
  val μ: (Functor.is[On] ∘ Functor.is[On]) ~> On

  val ι: Functor.Identity[OnCat] ~> On
}

final class KleisliCategory[M <: Monad](val monad: Monad.is[M])
    extends Category {

  type BaseCat = M#On#Source
  val baseCat: Category.is[BaseCat] =
    monad.on.source

  type Objects =
    M#On#Source#Objects

  type C[X <: Objects, Y <: Objects] =
    BaseCat#C[X, M#On#F[Y]]

  type F[X <: Objects] = M#On#F[X]

  @inline final def identity[X <: Objects]: C[X, X] =
    monad.ι[X]

  @inline final def composition[
      X <: Objects,
      Y <: Objects,
      Z <: Objects
  ]: C[X, Y] × C[Y, Z] -> C[X, Z] =
    λ { fg =>
      Category(baseCat) ⊢ {
        fg.left >=> monad.on(fg.right) >=> monad.μ[Z]
      }
    }
}

object KleisliCategory {

  @inline final def apply[Mnd <: Monad]: Monad.is[Mnd] -> KleisliCategory[Mnd] =
    λ { new KleisliCategory(_) }
}

object Monad {

  type is[M <: Monad] =
    M {
      type On = M#On
    }

  type on[F0 <: Functor { type Target = Source }] =
    Monad { type On = F0 }

  final class Identity[Cat <: Category](
      val on: Functor.is[Functor.Identity[Cat]])
      extends Monad {

    type On = Functor.Identity[Cat]

    object μ extends Between(on >-> on, on) {

      def apply[X <: SourceCategory#Objects]: TargetCategory#C[X, X] =
        on.source.identity
    }

    val ι =
      NaturalTransformation.identity(on)
  }

  @inline final def identity[Cat <: Category]
    : Functor.is[Functor.Identity[Cat]] -> is[Identity[Cat]] =
    λ { new Identity(_) }
}
