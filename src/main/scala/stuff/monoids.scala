package ohnosequences.stuff

abstract class Monoid {

  type In <: MonoidalCategory

  type M <: In#On#Objects

  val unit: MonoidalCategory.is[In]#On#C[In#I, M]

  val multiplication: MonoidalCategory.is[In]#On#C[In# ⊗[M, M], M]
}

object Monoid {

  type In[MC <: MonoidalCategory] =
    Monoid { type In = MC }

  final
  class UnitMonoid[MCat <: MonoidalCategory](val mcat: MonoidalCategory.is[MCat]) extends Monoid {

    type In = MCat
    type M = MCat#I

    val unit =
      mcat.on.identity

    val multiplication =
      mcat.unitl
  }
}
