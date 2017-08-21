package ohnosequences.stuff

abstract class Monoid {

  type In <: MonoidalCategory

  type M <: In#On#Objects

  val unit: In#On#C[In#I, M]

  val multiplication: In#On#C[In# ⊗[M, M], M]
}

object Monoid {

  type In[MC <: MonoidalCategory] =
    Monoid { type In = MC }

  final
  class UnitMonoid[MCat <: MonoidalCategory](mcat: MCat) extends Monoid {

    type In = MCat
    type M = MCat#I

    val unit =
      Category.is(MonoidalCategory.is(mcat).on).identity

    val multiplication =
      MonoidalCategory.is(mcat).unitl
  }
}
