package ohnosequences.stuff

abstract class Monoid {

  type OnM <: MonoidalCategory

  type M <: OnM#On#Objects

  val unit: OnM#On#C[OnM#I, M]

  val multiplication: OnM#On#C[OnM# ⊗[M, M], M]
}

object Monoid {

  class UnitMonoid[MCat <: MonoidalCategory](mcat: MCat) extends Monoid {

    type OnM = MCat
    type M = MCat#I

    val unit =
      Category.is(MonoidalCategory.is(mcat).on).identity

    val multiplication =
      MonoidalCategory.is(mcat).unitl
  }
}
