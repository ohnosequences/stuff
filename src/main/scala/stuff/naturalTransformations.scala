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
  type >-> [
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

    @inline final
    def first = left(pair)

    @inline final
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
  : (is[A] × is[B]) -> (A >-> B) =
    λ { new VerticalComposition(_) }

  @infix
  type >=> [
    M <: NaturalTransformation,
    N <: NaturalTransformation {
      type SourceCategory = M#TargetCategory
    }
  ] = HorizontalComposition[M,N]

  final
  class HorizontalComposition[
    M <: NaturalTransformation,
    N <: NaturalTransformation { type SourceCategory = M#TargetCategory }
  ]
  (val pair: is[M] × is[N]) extends NaturalTransformation {

    @inline final
    def first = left(pair)

    @inline final
    def second = right(pair)

    type SourceCategory = M#SourceCategory
    val sourceCategory  = first.sourceCategory

    type SourceFunctor =
      Functor.Composition[
        is[M]#SourceFunctor,
        is[N]#SourceFunctor
      ]
    val sourceFunctor =
      Functor.composition(first.sourceFunctor and second.sourceFunctor)

    type TargetFunctor =
      Functor.Composition[
        is[M]#TargetFunctor,
        is[N]#TargetFunctor
      ]
    val targetFunctor =
      Functor.composition(first.targetFunctor and second.targetFunctor)

    type TargetCategory = N#TargetCategory
    val targetCategory  = second.targetCategory

    final
    def apply[X <: SourceObjects]
      : TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
        targetCategory.composition(
           second.sourceFunctor(first[X]) and second[is[M]#TargetFunctor#F[X]]
        )
  }

  @inline final
  def horizontalComposition[
    M <: NaturalTransformation,
    N <: NaturalTransformation { type SourceCategory = M#TargetCategory }
  ]
  : (is[M] × is[N]) -> (M >=> N) =
    λ { new HorizontalComposition(_) }
}
