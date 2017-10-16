package ohnosequences.stuff

import naturalTransformations._
import products._
import functions._

abstract
class Monad {

  type On <: Category
  val on   : Category.is[On]

  type OnF <: Functor { type Source = On; type Target = On }
  val onF   : Functor.is[OnF]

  val μ: Functor.is[Functor.Composition[OnF,OnF]] ~> Functor.is[OnF]

  val ι: Functor.is[Functor.Identity[On]] ~> Functor.is[OnF]
}

abstract
class KleisliCategory extends Category {

  // NOTE we need this, don't ask me why
  type BaseCat <: Category
  val baseCat: Category.is[BaseCat]

  type M <: Monad.on[BaseCat]
  val m: Monad.is[M]

  type Objects =
    BaseCat#Objects

  type C[X <: Objects, Y <: Objects] =
    M#On#C[X, M#OnF#F[Y]]

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

object Monad {

  type on[Cat <: Category] =
    Monad { type On = Cat }

  type is[M <: Monad] =
    M {
      type On   = M#On
      type OnF  = M#OnF
    }

  final
  class Identity[Cat <: Category](val on: Category.is[Cat]) extends Monad {

    type On = Cat

    type OnF = Functor.Identity[Cat]
    val onF  = Functor.identity(on)

    val μ =
      new NaturalTransformation {

        type SourceCategory = On
        val sourceCategory  = on

        type SourceFunctor = Functor.is[Functor.Composition[OnF,OnF]]
        val sourceFunctor  = Functor.composition(onF and onF)

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
