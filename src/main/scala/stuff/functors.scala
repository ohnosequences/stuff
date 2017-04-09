package ohnosequences.stuff

import functions._, products._
import scala.inline

abstract class Functor {

  type Source <: Category
  def source: Source

  type Target <: Category
  def target: Target

  type F[Z <: Source#Objects] <: Target#Objects

  def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]]
}

object functors {

  type is[functor <: Functor] =
    Functor {
      type Source = functor#Source
      type Target = functor#Target
      type F[Z <: functor#Source#Objects] = functor#F[Z]
    }

  @inline final
  def is[functor <: Functor](f: functor): is[functor] =
    f.asInstanceOf[is[functor]]

  type Identity[Cat <: Category] = Functor {

    type Source = Cat
    type Target = Cat

    type F[Z <: Cat#Objects] = Z
  }

  type Composition[F0 <: Functor, G0 <: Functor { type Source = F0#Target }] = Functor {

    type Source = F0#Source
    type Target = G0#Target

    type F[Z <: F0#Source#Objects] = G0#F[ F0#F[Z] ]
  }

  @inline final
  def composition[F0 <: Functor, G0 <: Functor { type Source = F0#Target }]: (F0 × G0) -> Composition[F0,G0] =
    λ { fg =>
      new Functor {

        @inline final
        val F0: F0 = left(fg)

        @inline final
        val G0: G0 = right(fg)

        type Source = F0#Source
        @inline final
        val source: Source = F0.source

        type Target = G0#Target
        @inline final
        val target: Target = G0.target

        type F[Z <: F0#Source#Objects] = G0#F[ F0#F[Z] ]

        @inline final
        def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
          is(F0).apply >-> is(G0).apply
      }
    }

  @inline final
  def identity[Cat <: Category]: Cat -> Identity[Cat] =
    λ { cat: Cat =>
      new Functor {

        @inline final
        type Source = Cat
        @inline final
        val source: Source = cat

        @inline final
        type Target = Cat
        @inline final
        val target: Target = cat

        @inline final
        type F[Z <: Cat#Objects] = Z

        @inline final
        def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
          Scala.identity
      }
    }
}
