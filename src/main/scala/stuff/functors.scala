package ohnosequences.stuff

import Function._

abstract class Functor {

  type Source <: Category
  def source: Source

  type Target <: Category
  def target: Target

  type F[Z <: Source#Objects] <: Target#Objects

  def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]]
}

object functors {

  type Identity[Cat <: Category] = Functor {

    type Source = Cat
    type Target = Cat

    type F[Z <: Cat#Objects] = Z
  }

  def identity[Cat <: Category]: Cat -> Identity[Cat] =
    Function {
      cat: Cat => new Functor {

        type Source = Cat
        val source: Source = cat
        type Target = Cat
        val target: Target = cat

        type F[Z <: Cat#Objects] = Z

        def apply[X <: Source#Objects, Y <: Source#Objects]: Source#C[X,Y] -> Target#C[F[X], F[Y]] =
          Scala.identity
      }
    }
}
