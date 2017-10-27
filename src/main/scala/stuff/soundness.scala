package soundness

import ohnosequences.stuff._, products._

case object kosher {

  type Unit =
    scala.Unit

  // classical unsoundness example, coming from https://github.com/scala/bug/issues/4377
  // not that applicable given our conventions
  // it'd be good to fix it anyway

  abstract class Animal {
    type Food
    def find: Food
    def eat: Food -> Unit
  }

  type is[A <: Animal] =
    A { type Food = A#Food }

  // is[A] works abstractly
  abstract class AnimalWithFood[X] extends Animal { type Food = X }

  def shouldWork[X, A <: AnimalWithFood[X]]: A -> is[A] =
    λ { a =>
      a
    }

  // NOTE you cannot write this directly
  // def exchangeFood[A <: Animal]: A × A -> Unit =
  //   λ { xy: A × A =>
  //
  //     val x = left(xy); val y = right(xy)
  //
  //     y.eat(x.find)
  // }

  // NOTE this is fine
  def exchangeFood[A <: Animal]: is[A] × is[A] -> Unit =
    λ { xy =>
      left(xy) eat right(xy).find
    }

  class Grass
  class Sheep extends Animal {

    type Food = Grass

    def find: Food =
      new Grass

    def eat: Food -> Unit =
      λ { _ =>
        ()
      }
  }
  class Wolf extends Animal {

    type Food = Sheep

    def find: Food =
      new Sheep

    def eat: Food -> Unit =
      λ { _ =>
        ()
      }
  }

  // Whoops we made sheep cannibals, and wolfs eat grass.
  // val z = exchangeFood(new Sheep and new Wolf)
  // ↑ NO! it won't compile

  // this is fine of course
  val z = exchangeFood(new Sheep and new Sheep)
}
