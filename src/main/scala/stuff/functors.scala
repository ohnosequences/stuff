package ohnosequences.stuff

import functions._, products._

abstract class Functor {

  type Source <: Category
  val source: Category.is[Source]

  type Target <: Category
  val target: Category.is[Target]

  type SourceObjects = Source#Objects
  type TargetObjects = Target#Objects

  type F[Z <: SourceObjects] <: TargetObjects

  def at[
    X <: Source#Objects,
    Y <: Source#Objects
  ]
  :  Category.is[Source]#C[X,Y] -> Target#C[F[X], F[Y]]
}

object Functor {

  type endo =
    Functor { type Source = Target }

  implicit final
  class FunctorSyntax[Fn <: Functor](val functor: is[Fn]) {

    def apply[X <: Fn#Source#Objects, Y <: Fn#Source#Objects](f: Fn#Source#C[X,Y]): Fn#Target#C[Fn#F[X], Fn#F[Y]] =
      functor.at[X,Y](f)
  }

  type between[Src <: Category, Tgt <: Category] =
    Functor {
      type Source = Src
      type Target = Tgt
    }

  type is[functor <: Functor] =
    functor {
      type Source                         = functor#Source
      type SourceObjects                  = functor#SourceObjects
      type Target                         = functor#Target
      type TargetObjects                  = functor#TargetObjects
      type F[Z <: functor#SourceObjects]  = functor#F[Z]
    }

  final
  class Identity[Cat <: Category](val cat: Category.is[Cat]) extends Functor {

    type Source = Cat
    val source = cat

    type Target = Cat
    val target = cat

    type F[Z <: Cat#Objects] = Z

    def at[X <: SourceObjects, Y <: SourceObjects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      Scala.identity
  }

  // @infix
  type ∘ [
    F0 <: Functor,
    G0 <: Functor { type Source = F0#Target }
  ] = Composition[F0, G0]

  final
  class Composition[
    F0 <: Functor,
    G0 <: Functor { type Source = F0#Target }
  ](
    val first : is[F0],
    val second: is[G0]
  )
  extends Functor {

    type Source = F0#Source
    val source = first.source

    type Target = G0#Target
    val target = second.target

    type F[Z <: F0#Source#Objects] = G0#F[ F0#F[Z] ]

    def at[
      X <: Source#Objects,
      Y <: Source#Objects
    ]
    : Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      first.at >-> second.at
  }

  def composition[F0 <: Functor, G0 <: Functor { type Source = F0#Target }]: (is[F0] × is[G0]) -> is[Composition[F0,G0]] =
    λ { fg => new Composition(left(fg), right(fg)).asInstanceOf[is[Composition[F0,G0]]] }

  // due to a bug
  def identity[Cat <: Category]: Category.is[Cat] -> Functor.is[Identity[Cat]] =
    λ { new Identity(_).asInstanceOf[Functor.is[Identity[Cat]]] }
}
