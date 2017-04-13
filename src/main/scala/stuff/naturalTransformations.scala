package ohnosequences.stuff

import functions._, products._

abstract class NaturalTransformation {

  type SourceCategory <: Category
  def sourceCategory: SourceCategory

  type TargetCategory <: Category
  def targetCategory: TargetCategory

  type SourceFunctor <: Functor { type Source = SourceCategory; type Target = TargetCategory }
  def sourceFunctor: SourceFunctor

  type TargetFunctor <: Functor { type Source = SourceCategory; type Target = TargetCategory }
  def targetFunctor: TargetFunctor

  def apply[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]]
}

case object naturalTransformations {

  type is[nat <: NaturalTransformation] =
    nat {
      type SourceCategory = nat#SourceCategory
      type TargetCategory = nat#TargetCategory

      type SourceFunctor = nat#SourceFunctor { type Source = SourceCategory; type Target = TargetCategory }
      type TargetFunctor = nat#TargetFunctor { type Source = SourceCategory; type Target = TargetCategory }
    }

  final
  def is[nat <: NaturalTransformation](n: nat): is[nat] =
    n.asInstanceOf[is[nat]]

  type Identity[functor <: Functor] =
    NaturalTransformation {

      type SourceCategory = functor#Source
      type TargetCategory = functor#Target

      type SourceFunctor = functors.is[functor]
      type TargetFunctor = functors.is[functor]
    }

  final
  def identity[functor <: Functor]: functor -> Identity[functor] =
    λ { F0: functor =>
      new NaturalTransformation {

        type SourceCategory = functor#Source
        val sourceCategory: SourceCategory = F0.source

        type TargetCategory = functor#Target
        val targetCategory: TargetCategory = F0.target

        type SourceFunctor = functors.is[functor]
        val sourceFunctor: SourceFunctor = functors.is(F0)
        type TargetFunctor = functors.is[functor]
        val targetFunctor: TargetFunctor = functors.is(F0)

        def apply[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
          Category.is(targetCategory).identity
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
  : (A × B) -> VerticalComposition[A,B] =
    λ { ab =>
      new NaturalTransformation {

        val a: A = left(ab); val b: B = right(ab)

        type SourceFunctor  = is[A]#SourceFunctor
        val sourceFunctor: SourceFunctor = is(a).sourceFunctor

        type SourceCategory = A#SourceCategory
        val sourceCategory: SourceCategory = a.sourceCategory

        type TargetFunctor  = is[B]#TargetFunctor
        val targetFunctor: TargetFunctor = is(b).targetFunctor

        type TargetCategory = B#TargetCategory
        val targetCategory: TargetCategory = b.targetCategory

        final
        def apply[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
          Category.is(targetCategory).composition( is(a).apply[X] and is(b).apply[X])
      }
    }




}
