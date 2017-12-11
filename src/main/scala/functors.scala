package ohnosequences.stuff

abstract class Functor { me =>

  type Source <: Category
  val source: Category.is[Source]

  type Target <: Category
  val target: Category.is[Target]

  type F[Z <: Source#Objects] <: Target#Objects

  def at[
      X <: Source#Objects,
      Y <: Source#Objects
  ]: Source#C[X, Y] -> Target#C[F[X], F[Y]]
}

object Functor {

  type is[functor <: Functor] =
    functor {
      type Source                 = functor#Source
      type Target                 = functor#Target
      type F[Z <: Source#Objects] = functor#F[Z]
    }

  implicit final def functorSyntax[Fn <: Functor](fn: Fn)(
      implicit ev: fn.type <:< is[Fn]): FunctorSyntax[Fn] =
    new FunctorSyntax(ev(fn))

  final class FunctorSyntax[Fn <: Functor](val functor: is[Fn])
      extends CompileTime {

    final def id: NaturalTransformation.Identity[Fn] =
      NaturalTransformation.identity[Fn](functor)

    @inline
    final def apply[X <: Fn#Source#Objects, Y <: Fn#Source#Objects](
        f: Fn#Source#C[X, Y]): Fn#Target#C[Fn#F[X], Fn#F[Y]] =
      functor.at[X, Y](f)

    @inline
    final def >->[Gn <: Functor { type Source = Fn#Target }](other: Gn)(
        implicit ev: other.type <:< is[Gn]): is[Composition[is[Fn], is[Gn]]] =
      composition[is[Fn], is[Gn]](functor and ev(other))

    @inline
    final def ∘[Gn <: Functor { type Target = Fn#Source }](other: Gn)(
        implicit ev: other.type <:< is[Gn]): is[Composition[is[Gn], is[Fn]]] =
      composition[is[Gn], is[Fn]](ev(other) and functor)
  }

  type between[Src <: Category, Tgt <: Category] =
    Functor {
      type Source = Src
      type Target = Tgt
    }

  type Endo = Functor { type Source = Target }

  @infix
  type ∘[
      F0 <: Functor,
      G0 <: Functor { type Target = F0#Source }
  ] = Composition[is[G0], is[F0]]
  //////////////////////////////////////////////////////////////////////////////
  // Functor composition
  //////////////////////////////////////////////////////////////////////////////
  type Composition[
      F0 <: Functor,
      G0 <: Functor { type Source = F0#Target }
  ] = CompositionImpl {

    type Source                    = F0#Source
    type Target                    = G0#Target
    type F[Z <: F0#Source#Objects] = G0#F[F0#F[Z]]
  }

  @inline
  final def composition[
      F0 <: Functor,
      G0 <: Functor { type Source = F0#Target }
  ]: (is[F0] × is[G0]) -> is[Composition[F0, G0]] =
    λ { fg =>
      new CompositionImpl {
        type Source                    = F0#Source
        type Target                    = G0#Target
        type F[Z <: F0#Source#Objects] = G0#F[F0#F[Z]]

        val first: is[F0] =
          fg.left
        val second: is[G0] =
          fg.right

        val source = first.source
        val target = second.target

        def at[
            X <: Source#Objects,
            Y <: Source#Objects
        ]: Source#C[X, Y] -> Target#C[F[X], F[Y]] =
          first.at >-> second.at
      }
    }

  sealed abstract class CompositionImpl extends Functor
  //////////////////////////////////////////////////////////////////////////////
  // Identity functors
  //////////////////////////////////////////////////////////////////////////////
  type Identity[Cat <: Category] = IdentityImpl {

    type Source                 = Cat
    type Target                 = Cat
    type F[Z <: Source#Objects] = Z
  }

  def identity[Cat <: Category]: Category.is[Cat] -> is[Identity[Cat]] =
    λ { cat =>
      new IdentityImpl {
        type Source = Cat
        val source = cat
        type Target = Cat
        val target = cat
        type F[Z <: Source#Objects] = Z
        def at[X <: Source#Objects, Y <: Source#Objects]
          : Source#C[X, Y] -> Target#C[F[X], F[Y]] =
          Scala.identity
      }
    }

  sealed abstract class IdentityImpl extends Functor
  //////////////////////////////////////////////////////////////////////////////
}
