package ohnosequences.stuff

import functions._, products._

abstract
class Functor { me =>

  type Source <: Category
  val source: Category.is[Source]

  final
  type SourceObjects =
    Category.is[Source]#Objects

  type Target <: Category
  val target: Category.is[Target]

  type F[Z <: SourceObjects] <: Target#Objects

  def at[X <: SourceObjects, Y <: SourceObjects]: Source#C[X,Y] -> Target#C[F[X], F[Y]]
}

object Functor {

  implicit final
  class FunctorSyntax[Fn <: Functor](val functor: is[Fn]) extends scala.AnyVal {

    def apply[X <: Fn#SourceObjects, Y <: Fn#SourceObjects](f: Fn#Source#C[X,Y]): Fn#Target#C[Fn#F[X], Fn#F[Y]] =
      functor.at[X,Y](f)

    def >->[Gn <: Functor { type Source = Fn#Target }](other: is[Gn])
      : Composition[Fn, Gn] =
        new Composition(functor, other)
  }

  type between[Src <: Category, Tgt <: Category] =
    Functor {
      type Source = Src
      type Target = Tgt
    }

  // final
  // type is[functor <: Functor] =
  //   functor {
  //     type Source                 = functor # Source
  //     type Target                 = functor # Target
  //     type SourceObjects          = functor # SourceObjects
  //     type F[Z <: SourceObjects]  = functor # F[Z]
  //   }

  type is[functor <: Functor] >:
    functor {
      type Source                 = functor # Source
      type Target                 = functor # Target
      type SourceObjects          = functor # SourceObjects
      type F[Z <: SourceObjects]  = functor # F[Z]
    }
    <: functor {
      type Source                 = functor # Source
      type Target                 = functor # Target
      type SourceObjects          = functor # SourceObjects
      type F[Z <: SourceObjects]  = functor # F[Z]
    }

  final
  class Identity[Cat <: Category](cat: Category.is[Cat]) extends Functor {

    type Source = Cat
    val source = cat

    type Target = Cat
    val target = cat

    type F[Z <: SourceObjects] = Z

    def at[X <: SourceObjects, Y <: SourceObjects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      Scala.identity
  }

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

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      first.at >-> second.at
  }

  def composition[F0 <: Functor, G0 <: Functor { type Source = F0#Target }]: (is[F0] × is[G0]) -> Composition[F0,G0] =
    λ { fg => new Composition(left(fg), right(fg)) }

  def identity[Cat <: Category]: Category.is[Cat] -> is[Identity[Cat]] =
    λ {
      new Identity(_)
        .asInstanceOf[ is[Identity[Cat]] ]
    }
}
