package ohnosequences.stuff

abstract class NaturalTransformation { nat =>

  type SourceFunctor <: Functor
  val sourceFunctor: Functor.is[SourceFunctor]

  type TargetFunctor <: Functor {
    type Source = SourceFunctor#Source
    type Target = SourceFunctor#Target
  }
  val targetFunctor: Functor.is[TargetFunctor]

  def apply[X <: SourceFunctor#Source#Objects]
    : SourceFunctor#Target#C[SourceFunctor#F[X], TargetFunctor#F[X]]
}

object NaturalTransformation {

  type is[nat <: NaturalTransformation] =
    nat {
      type SourceFunctor = nat#SourceFunctor
      type TargetFunctor = nat#TargetFunctor {
        type Source = SourceFunctor#Source
        type Target = SourceFunctor#Target
      }
    }

  type ~>[F1 <: Functor,
          F2 <: Functor {
            type Source = F1#Source
            type Target = F1#Target
          }] =
    NaturalTransformation {
      type SourceFunctor = F1
      type TargetFunctor = F2
    }

  final class Identity[Fnctr <: Functor](val fnctr: Functor.is[Fnctr])
      extends NaturalTransformation {

    final type SourceFunctor = Functor.is[Fnctr]
    val sourceFunctor: Functor.is[Fnctr] = fnctr

    final type TargetFunctor = Functor.is[Fnctr]
    val targetFunctor: Functor.is[Fnctr] = fnctr

    @inline final def apply[X <: SourceFunctor#Source#Objects]
      : TargetFunctor#Target#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
      targetFunctor.target.identity
  }

  @inline
  final def identity[Fnctr <: Functor]: Functor.is[Fnctr] -> Identity[Fnctr] =
    λ { new Identity(_) }

  // syntax
  ///////////////////////////////////////////////////////////////////////////
  implicit final def syntaxNaturalTransformation[N <: NaturalTransformation](
      n: N)(implicit ev: n.type <:< is[N]): Syntax[N] =
    new Syntax(ev(n))

  final class Syntax[N <: NaturalTransformation](val n: is[N])
      extends CompileTime {

    // vertical
    @inline
    final def >->[M <: NaturalTransformation {
      type SourceFunctor = is[N]#TargetFunctor
    }](m: M)(implicit ev: m.type <:< is[M]): N >-> M =
      verticalComposition(n and ev(m))

    @inline
    final def andThen[M <: NaturalTransformation {
      type SourceFunctor = is[N]#TargetFunctor
    }](m: is[M]): N >-> M =
      verticalComposition(n and m)
  }

  abstract class Between[
      F1 <: Functor,
      F2 <: Functor { type Source = F1#Source; type Target = F1#Target }
  ](
      val sourceFunctor: Functor.is[F1],
      val targetFunctor: Functor.is[F2]
  ) extends NaturalTransformation {

    type SourceFunctor = F1
    type TargetFunctor = F2
  }

  @infix
  type >->[
      A <: NaturalTransformation,
      B <: NaturalTransformation {
        type SourceFunctor = is[A]#TargetFunctor
      }
  ] =
    VerticalComposition[A, B]

  final class VerticalComposition[
      A <: NaturalTransformation,
      B <: NaturalTransformation {
        type SourceFunctor = is[A]#TargetFunctor
      }
  ](val pair: is[A] × is[B])
      extends NaturalTransformation {

    @inline
    final def first: is[A] =
      pair.left

    @inline
    final def second: is[B] =
      pair.right

    type SourceFunctor =
      A#SourceFunctor

    val sourceFunctor: Functor.is[SourceFunctor] =
      first.sourceFunctor

    type TargetFunctor = B#TargetFunctor
    val targetFunctor: Functor.is[TargetFunctor] =
      second.targetFunctor.asInstanceOf[Functor.is[B#TargetFunctor]]

    final def apply[X <: SourceFunctor#Source#Objects]
      : SourceFunctor#Target#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
      Category(sourceFunctor.target) ⊢ { first[X] >=> second[X] }
  }

  def verticalComposition[
      A <: NaturalTransformation,
      B <: NaturalTransformation {
        type SourceFunctor = is[A]#TargetFunctor
      }
  ]: (is[A] × is[B]) -> (A >-> B) =
    λ { new VerticalComposition(_) }

  @infix
  type >=>[
      M <: NaturalTransformation,
      N <: NaturalTransformation {
        type SourceFunctor <: Functor { type Source = M#SourceFunctor#Target }
      }
  ] = HorizontalComposition[M, N]

  final class HorizontalComposition[
      M <: NaturalTransformation,
      N <: NaturalTransformation {
        type SourceFunctor <: Functor { type Source = M#SourceFunctor#Target }
      }
  ](val pair: is[M] × is[N])
      extends NaturalTransformation {

    @inline
    final def first: is[M] =
      pair.left

    @inline
    final def second: is[N] =
      pair.right

    type SourceFunctor =
      Functor.Composition[
        is[M]#SourceFunctor,
        is[N]#SourceFunctor
      ]
    val sourceFunctor: Functor.is[SourceFunctor] =
      Functor.composition(first.sourceFunctor and second.sourceFunctor)

    type TargetFunctor =
      Functor.Composition[
        is[M]#TargetFunctor,
        is[N]#TargetFunctor
      ]
    val targetFunctor: Functor.is[TargetFunctor] =
      Functor.composition(first.targetFunctor and second.targetFunctor)

    final def apply[X <: SourceFunctor#Source#Objects]
      : SourceFunctor#Target#C[SourceFunctor#F[X], TargetFunctor#F[X]] =
      Category(sourceFunctor.target) ⊢ {
        (second.sourceFunctor at first[X]) >=> second[is[M]#TargetFunctor#F[X]]
      }
  }

  @inline
  final def horizontalComposition[
      M <: NaturalTransformation,
      N <: NaturalTransformation {
        type SourceFunctor <: Functor { type Source = M#SourceFunctor#Target }
      }
  ]: (is[M] × is[N]) -> (M >=> N) =
    λ { new HorizontalComposition(_) }
}
