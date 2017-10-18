package ohnosequences.stuff

import functions._, products._

abstract
class NaturalTransformation {

  type SourceCategory <: Category
  val sourceCategory   : Category.is[SourceCategory]

  type TargetCategory <: Category
  val targetCategory   : Category.is[TargetCategory]

  type SourceFunctor <: Functor.between[SourceCategory, TargetCategory]
  val sourceFunctor   : Functor.is[SourceFunctor]

  type TargetFunctor <: Functor.between[SourceCategory, TargetCategory]
  val targetFunctor   : Functor.is[TargetFunctor]

  final
  type SourceObjects =
    SourceCategory#Objects

  def apply[X <: SourceObjects]
    : TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]]
}

object NaturalTransformation {

  type ~>[F1 <: Functor, F2 <: Functor] =
    NaturalTransformation {
      type SourceFunctor = F1
      type TargetFunctor = F2
    }

  type is[nat <: NaturalTransformation] =
    nat {
      type SourceCategory = nat#SourceCategory
      type TargetCategory = nat#TargetCategory

      type SourceObjects = nat#SourceObjects

      type SourceFunctor  = nat#SourceFunctor {
        type Source = nat#SourceCategory
        type Target = nat#TargetCategory
      }
      type TargetFunctor = nat#TargetFunctor {
        type Source = nat#SourceCategory
        type Target = nat#TargetCategory
      }
    }

  final
  class Identity[Fnctr <: Functor](val fnctr: Functor.is[Fnctr])
  extends NaturalTransformation {

    type SourceCategory = Fnctr#Source
    val sourceCategory  = fnctr.source

    type TargetCategory = Fnctr#Target
    val targetCategory  = fnctr.target

    type SourceFunctor = Functor.is[Fnctr]
    val sourceFunctor  = fnctr

    type TargetFunctor = Functor.is[Fnctr]
    val targetFunctor  = fnctr

    def apply[X <: SourceObjects]
      : TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
        targetCategory.identity
  }

  @infix
  type * [
    A <: NaturalTransformation,
    B <: NaturalTransformation {
      type SourceFunctor  = A#TargetFunctor
      type SourceCategory = A#SourceCategory
      type TargetCategory = A#TargetCategory
    }
  ] =
    VerticalComposition[A, B]

  final
  class VerticalComposition[
    A <: NaturalTransformation,
    B <: NaturalTransformation {
      type SourceFunctor  = A#TargetFunctor
      type SourceCategory = A#SourceCategory
      type TargetCategory = A#TargetCategory
    }
  ](val pair: is[A] × is[B]) extends NaturalTransformation {

    @inline private
    def first = left(pair)

    @inline private
    def second = right(pair)

    type SourceCategory = is[A]#SourceCategory
    val sourceCategory  = first.sourceCategory

    type SourceFunctor  = is[A]#SourceFunctor
    val sourceFunctor   = first.sourceFunctor

    type TargetFunctor  = is[B]#TargetFunctor
    val targetFunctor   = second.targetFunctor

    type TargetCategory = is[B]#TargetCategory
    val targetCategory  = second.targetCategory

    final
    def apply[X <: SourceObjects]
      : TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
        targetCategory.composition( first[X] and second[X])
  }

  def verticalComposition[
    A <: NaturalTransformation,
    B <: NaturalTransformation {
      type SourceFunctor  = A#TargetFunctor
      type SourceCategory = A#SourceCategory
      type TargetCategory = A#TargetCategory
    }
  ]
  : (is[A] × is[B]) -> (A * B) =
    λ { new VerticalComposition(_) }

  final
  class HorizontalComposition[
    M <: NaturalTransformation,
    N <: NaturalTransformation { type SourceCategory = M#TargetCategory }
  ]
  (val m: is[M], val n: is[N]) extends NaturalTransformation {

    type SourceCategory = M#SourceCategory
    val sourceCategory  = m.sourceCategory

    type SourceFunctor =
      Functor.Composition[
        is[M]#SourceFunctor,
        is[N]#SourceFunctor
      ]
    val sourceFunctor =
      Functor.composition(m.sourceFunctor and n.sourceFunctor)

    type TargetFunctor =
      Functor.Composition[
        is[M]#TargetFunctor,
        is[N]#TargetFunctor
      ]
    val targetFunctor =
      Functor.composition(m.targetFunctor and n.targetFunctor)


    type TargetCategory = N#TargetCategory
    val targetCategory  = n.targetCategory

    final
    def apply[X <: SourceCategory#Objects]
      : TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
        targetCategory.composition(
           n.sourceFunctor(m[X]) and n[is[M]#TargetFunctor#F[X]]
        )
  }

  def horizontalComposition[
    M <: NaturalTransformation,
    N <: NaturalTransformation { type SourceCategory = M#TargetCategory }
  ]
  : (is[M] × is[N]) -> is[HorizontalComposition[M,N]] =
    λ { mn =>
      new HorizontalComposition(left(mn), right(mn))
        .asInstanceOf[is[HorizontalComposition[M,N]]]
    }
}
