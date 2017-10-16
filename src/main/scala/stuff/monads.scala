package ohnosequences.stuff

import naturalTransformations._
import products._

abstract
class Monad {

  type On <: Category
  val on: Category.is[On]

  type OnF <: Functor.between[On,On]
  val onF: Functor.is[OnF]

  // type Mu <: Functor.Composition[OnF,OnF] ~> OnF
  val μ: Functor.is[Functor.Composition[OnF,OnF]] ~> Functor.is[OnF]

  // type Unit <:
  val ι: Functor.is[Functor.Identity[On]] ~> Functor.is[OnF]
}

object Monad {

  abstract
  class Identity[Cat <: Category](val on: Category.is[Cat]) extends Monad {

    type On = Cat

    type OnF = Functor.Identity[Cat]
    val onF =
      Functor.identityB(on).asInstanceOf[Functor.is[OnF]]

    val μ =
      new NaturalTransformation {

        type SourceCategory = On
        val sourceCategory = on

        type SourceFunctor = Functor.is[Functor.Composition[OnF,OnF]]
        val sourceFunctor = Functor.composition(onF and onF)

        type TargetCategory = On
        val targetCategory = on

        type TargetFunctor = Functor.is[OnF]
        val targetFunctor = onF

        def apply[X <: On#Objects]: On#C[X,X] =
          on.identity
      }

    val ι =
      naturalTransformations.identity(onF)
  }
}
