package ohnosequences.stuff

import naturalTransformations._
import products._
import functions._

abstract
class Monad {

  type On = OnF#Source
  val on   : Category.is[On]

  type OnF <: Functor.EndoFunctor
  val onF   : Functor.is[OnF]

  // TODO do I need another Functor.is here?
  val μ: Functor.Composition[Functor.is[OnF],Functor.is[OnF]] ~> Functor.is[OnF]

  val ι: Functor.is[Functor.Identity[On]] ~> Functor.is[OnF]
}

abstract
class KleisliCategory extends Category {

  // NOTE we need this, don't ask me why
  type BaseCat = M#On
  lazy
  val baseCat: Category.is[BaseCat] =
    m.on

  type M <: Monad
  val m: Monad.is[M]

  type Objects =
    M#OnF#Source#Objects

  type C[X <: Objects, Y <: Objects] =
    BaseCat#C[X, M#OnF#F[Y]]

  def identity[X <: Objects]: C[X,X] =
    m.ι[X]

  def composition[
    X <: Objects,
    Y <: Objects,
    Z <: Objects
  ]
  : C[X,Y] × C[Y,Z] -> C[X,Z] =
    λ { fg =>
      baseCat.composition(
        left(fg) and baseCat.composition(m.onF(right(fg)) and m.μ[Z])
      )
    }
}

object KleisliCategory {

  def apply[Mnd <: Monad](mnd: Monad.is[Mnd]): KleisliCategory { type M = Mnd } =
    new KleisliCategory {
      type M = Mnd
      val m  = mnd
    }
}

object Monad {

  type on[Cat <: Category] =
    Monad { type On = Cat }

  type is[M <: Monad] =
    M {
      type OnF  = M#OnF
    }

  final
  class Identity[Cat <: Category](val on: Category.is[Cat]) extends Monad {

    type OnF = Functor.Identity[Cat]
    val onF  = Functor.identity(on)

    val μ =
      new NaturalTransformation {

        type SourceCategory = On
        val sourceCategory  = on

        type SourceFunctor =
          Functor.Composition[Functor.is[OnF],Functor.is[OnF]]

        val sourceFunctor: Functor.is[SourceFunctor] =
          Functor.composition(onF and onF)

        type TargetCategory = On
        val targetCategory  = on

        type TargetFunctor = Functor.is[OnF]
        val targetFunctor  = onF

        def apply[X <: On#Objects]: On#C[X,X] =
          on.identity
      }

    val ι =
      naturalTransformations.identity(onF)
  }

  @inline final
  def identity[Cat <: Category]: Category.is[Cat] -> Identity[Cat] =
    λ { new Identity(_) }
}
