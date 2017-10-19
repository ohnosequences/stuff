package ohnosequences.stuff

import NaturalTransformation._
import products._
import functions._
import Functor.{ ∘ }

abstract
class Monad {

  type On <: Functor { type Target = Source }
  val on   : Functor.is[On]

  final
  type OnCat = On#Source

  val μ: (Functor.is[On] ∘ Functor.is[On]) ~> Functor.is[On]

  val ι: Functor.is[Functor.Identity[OnCat]] ~> Functor.is[On]
}

final
class KleisliCategory[M <: Monad](val monad: Monad.is[M]) extends Category {

  type BaseCat = M#On#Source
  val baseCat: Category.is[BaseCat] =
    monad.on.source

  type Objects =
    M#On#Source#Objects

  type C[X <: Objects, Y <: Objects] =
    BaseCat#C[X, M#On#F[Y]]

  @inline final
  def identity[X <: Objects]: C[X,X] =
    monad.ι[X]

  @inline final
  def composition[
    X <: Objects,
    Y <: Objects,
    Z <: Objects
  ]
  : C[X,Y] × C[Y,Z] -> C[X,Z] =
    λ { fg =>
      baseCat.composition(
        left(fg) and baseCat.composition(monad.on(right(fg)) and monad.μ[Z])
      )
    }
}

object KleisliCategory {

  @inline final
  def apply[Mnd <: Monad]: Monad.is[Mnd] -> KleisliCategory[Mnd] =
    λ { new KleisliCategory(_) }
}

object Monad {

  type is[M <: Monad] =
    M {
      type On = M#On
    }

  type on[F0 <: Functor.endo] =
    Monad { type On = F0 }

  final
  class Identity[Cat <: Category](val on: Functor.is[Functor.Identity[Cat]])
  extends Monad {

    type On = Functor.Identity[Cat]

    val μ =
      new NaturalTransformation {

        type SourceCategory = Cat
        val sourceCategory  = on.source
        type TargetCategory = Cat
        val targetCategory  = on.target

        type SourceFunctor =
          Functor.Composition[Functor.is[On], Functor.is[On]]

        val sourceFunctor: Functor.is[SourceFunctor] =
          Functor.composition[Functor.is[On], Functor.is[On]](
            on and on
          )

        type TargetFunctor = Functor.is[On]
        val targetFunctor  = on

        def apply[X <: SourceCategory#Objects]: TargetCategory#C[X,X] =
          on.source.identity
      }

    val ι =
      NaturalTransformation.identity(on)
  }

  @inline final
  def identity[Cat <: Category]: Functor.is[Functor.Identity[Cat]] -> is[Identity[Cat]] =
    λ { new Identity(_) }
}
