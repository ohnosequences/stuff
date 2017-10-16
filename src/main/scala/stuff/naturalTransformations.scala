package ohnosequences.stuff

import functions._, products._

abstract class NaturalTransformation {

  type SourceCategory <: Category
  val sourceCategory: Category.is[SourceCategory]

  type TargetCategory <: Category
  val targetCategory: Category.is[TargetCategory]

  type SourceFunctor <: Functor {
    type Source = SourceCategory
    type Target = TargetCategory
  }

  val sourceFunctor: Functor.is[SourceFunctor]

  type TargetFunctor <: Functor {
    type Source = SourceCategory
    type Target = TargetCategory
  }

  val targetFunctor: Functor.is[TargetFunctor]

  def apply[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]]
}

object naturalTransformations {

  // NOTE bounds are not checked in type aliases
  // we can use this to our advantage here
  type ~>[F0 <: Functor, G0 <: Functor] =
    NaturalTransformation {

      type SourceCategory = F0#Source
      type TargetCategory = F0#Target
      type SourceFunctor = F0
      type TargetFunctor = G0
    }

  type is[nat <: NaturalTransformation] =
    nat {
      type SourceCategory = nat#SourceCategory
      type TargetCategory = nat#TargetCategory

      type SourceFunctor = nat#SourceFunctor { type Source = SourceCategory; type Target = TargetCategory }
      type TargetFunctor = nat#TargetFunctor { type Source = SourceCategory; type Target = TargetCategory }
    }

  type Identity[functor <: Functor] =
    NaturalTransformation {

      type SourceCategory = functor#Source
      type TargetCategory = functor#Target

      type SourceFunctor = Functor.is[functor]
      type TargetFunctor = Functor.is[functor]
    }

  final
  def identity[functor <: Functor]: Functor.is[functor] -> Identity[functor] =
    λ { F0: Functor.is[functor] =>
      new NaturalTransformation {

        type SourceCategory = functor#Source
        val sourceCategory = F0.source

        type TargetCategory = functor#Target
        val targetCategory = F0.target

        type SourceFunctor = Functor.is[functor]
        val sourceFunctor = F0

        type TargetFunctor = Functor.is[functor]
        val targetFunctor = F0

        def apply[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
          targetCategory.identity
      }
    }

  type VerticalComposition[
    A <: NaturalTransformation,
    B <: NaturalTransformation {
      type SourceFunctor  = A#TargetFunctor
      type SourceCategory = A#SourceCategory
      type TargetCategory = A#TargetCategory
    }
  ] =
    NaturalTransformation {

      type SourceFunctor = is[A]#SourceFunctor
      type SourceCategory = A#SourceCategory

      type TargetFunctor  = is[B]#TargetFunctor
      type TargetCategory = B#TargetCategory
    }

  final
  def verticalComposition[
    A <: NaturalTransformation,
    B <: NaturalTransformation {
      type SourceFunctor  = A#TargetFunctor
      type SourceCategory = A#SourceCategory
      type TargetCategory = A#TargetCategory
    }
  ]
  : (is[A] × is[B]) -> VerticalComposition[A,B] =
    λ { ab =>
      new NaturalTransformation {

        val a = left(ab)
        val b = right(ab)

        type SourceFunctor  = is[A]#SourceFunctor
        val sourceFunctor = a.sourceFunctor

        type SourceCategory = A#SourceCategory
        val sourceCategory = a.sourceCategory

        type TargetFunctor  = is[B]#TargetFunctor
        val targetFunctor = b.targetFunctor

        type TargetCategory = B#TargetCategory
        val targetCategory = b.targetCategory

        final
        def apply[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
          targetCategory.composition( a.apply[X] and b.apply[X])
          // targetCategory ⊢ { a.at >-> b.at } // would be nice
      }
    }




}
