package ohnosequences.stuff

import Function._
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

  type Identity[Cat <: Category] = Functor {

    type Source = Cat
    type Target = Cat

    type F[Z <: Cat#Objects] = Z
  }

  def identity[Cat <: Category]: Cat -> Identity[Cat] =
    Function {
      cat: Cat => new Functor {

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
