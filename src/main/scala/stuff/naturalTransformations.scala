package ohnosequences.stuff

import functions._, products._

abstract class NaturalTransformation {

  type SourceCategory <: Category
  val sourceCategory   : Category.is[SourceCategory]

  type TargetCategory <: Category
  val targetCategory   : Category.is[TargetCategory]

  type SourceFunctor <: Functor.between[SourceCategory, TargetCategory]
  val sourceFunctor   : Functor.is[SourceFunctor]

  type TargetFunctor <: Functor.between[SourceCategory, TargetCategory]
  val targetFunctor   : Functor.is[TargetFunctor]

  def apply[X <: SourceCategory#Objects]
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

      type SourceFunctor = nat#SourceFunctor {
        type Source = nat#SourceCategory
        type Target = nat#TargetCategory
      }
      type TargetFunctor = nat#TargetFunctor {
        type Source = nat#SourceCategory
        type Target = nat#TargetCategory
      }
    }

  class Identity[Fnctr <: Functor](val fnctr: Functor.is[Fnctr])
  extends NaturalTransformation {

    type SourceCategory = Fnctr#Source
    val sourceCategory = fnctr.source

    type TargetCategory = Fnctr#Target
    val targetCategory = fnctr.target

    type SourceFunctor = Functor.is[Fnctr]
    val sourceFunctor = fnctr

    type TargetFunctor = Functor.is[Fnctr]
    val targetFunctor = fnctr

    def apply[X <: SourceCategory#Objects]: TargetCategory#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
      targetCategory.identity
  }

  final
  class HorizontalComposition[
    M <: NaturalTransformation,
    N <: NaturalTransformation { type SourceCategory = M#TargetCategory }
  ]
  (val m: is[M], val n: is[N]) extends NaturalTransformation {

    type SourceCategory = M#SourceCategory
    val sourceCategory = m.sourceCategory

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
    val targetCategory = n.targetCategory

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

object naturalTransformations {

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
