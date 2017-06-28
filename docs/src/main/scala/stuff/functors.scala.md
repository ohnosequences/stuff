
```scala
package ohnosequences.stuff

import functions._, products._

abstract class Functor {

  type Source <: Category
  def source: Source

  type Target <: Category
  def target: Target

  type F[Z <: Source#Objects] <: Target#Objects

  def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]]
}

object functors {

  implicit class FunctorSyntax[Fn <: Functor](val functor: Fn) {

    def apply[X <: Fn#Source#Objects, Y <: Fn#Source#Objects](f: Fn#Source#C[X,Y]): Fn#Target#C[Fn#F[X], Fn#F[Y]] =
      is(functor).at[X,Y](f)
  }

  type between[Src <: Category, Tgt <: Category] =
    Functor {
      type Source = Src
      type Target = Tgt
    }

  type is[functor <: Functor] =
    functor {
      type Source = functor#Source
      type Target = functor#Target
      type F[Z <: functor#Source#Objects] = functor#F[Z]
    }

  def is[functor <: Functor](f: functor): is[functor] =
    f.asInstanceOf[is[functor]]

  case class Identity[Cat <: Category](cat: Cat) extends Functor {

    type Source = Cat
    val source = cat
    type Target = Cat
    val target = cat

    type F[Z <: Cat#Objects] = Z

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      Scala.identity
  }

  case class Composition[
    F0 <: Functor,
    G0 <: Functor { type Source = F0#Target }
  ](
    val first: F0,
    val second: G0
  )
  extends Functor {

    type Source = F0#Source
    val source: Source = first.source

    type Target = G0#Target
    val target: Target = second.target

    type F[Z <: F0#Source#Objects] = G0#F[ F0#F[Z] ]

    def at[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
      is(first).at >-> is(second).at
  }

  def composition[F0 <: Functor, G0 <: Functor { type Source = F0#Target }]: (F0 × G0) -> Composition[F0,G0] =
    λ { fg => Composition(left(fg), right(fg)) }

  def identity[Cat <: Category]: Cat -> Identity[Cat] =
    λ { cat: Cat => Identity(cat) }
}

```




[test/scala/tuples/stdComparison.scala]: ../../../test/scala/tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: ../../../test/scala/tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: ../../../test/scala/functors/functorExamples.scala.md
[test/scala/sums.scala]: ../../../test/scala/sums.scala.md
[test/scala/ScalaCategory.scala]: ../../../test/scala/ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: ../../../test/scala/functions/syntax.scala.md
[test/scala/categories.scala]: ../../../test/scala/categories.scala.md
[main/scala/stuff/products.scala]: products.scala.md
[main/scala/stuff/Scala.scala]: Scala.scala.md
[main/scala/stuff/package.scala]: package.scala.md
[main/scala/stuff/sums.scala]: sums.scala.md
[main/scala/stuff/boolean.scala]: boolean.scala.md
[main/scala/stuff/functors.scala]: functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: categories.scala.md
[main/scala/stuff/functions.scala]: functions.scala.md