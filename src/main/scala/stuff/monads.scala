package ohnosequences.stuff

import naturalTransformations._
import products._
import functions._

abstract
class Monad {

  type On <: Functor.endo
  val on   : Functor.is[On]

  // TODO do I need another Functor.is here?
  val μ: Functor.Composition[Functor.is[On],Functor.is[On]] ~> Functor.is[On]

  val ι: Functor.is[Functor.Identity[On#Source]] ~> Functor.is[On]
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
  class Identity[Cat <: Category](val cat: Category.is[Cat]) extends Monad {

    type On = Functor.Identity[Cat]
    val on  = Functor.identity(cat)

    val μ =
      new NaturalTransformation {

        type SourceCategory = On#Source
        val sourceCategory  = on.source

        type SourceFunctor =
          Functor.Composition[Functor.is[On], Functor.is[On]]

        val sourceFunctor: Functor.is[SourceFunctor] =
          Functor.composition(on and on)

        type TargetCategory = On#Target
        val targetCategory  = on.target

        type TargetFunctor = Functor.is[On]
        val targetFunctor  = on

        def apply[X <: SourceCategory#Objects]: TargetCategory#C[X,X] =
          cat.identity
      }

    val ι =
      naturalTransformations.identity(on)
  }

  @inline final
  def identity[Cat <: Category]: Category.is[Cat] -> is[Identity[Cat]] =
    λ { new Identity(_) }
}
