package ohnosequences.stuff

abstract class Monoid {

  type In <: MonoidalCategory

  type M <: In#On#Objects

  val unit: MonoidalCategory.is[In]#On#C[In#I, M]

  // format: off
  val multiplication: MonoidalCategory.is[In]#On#C[In # ⊗[M, M], M]
  // format: on
}

object Monoid {

  type In[MC <: MonoidalCategory] =
    Monoid { type In = MC }

  final class UnitMonoid[MCat <: MonoidalCategory](
      val mcat: MonoidalCategory.is[MCat])
      extends Monoid {

    type In = MCat
    type M  = MCat#I

    val unit =
      mcat.on.identity

    val multiplication =
      mcat.unitl
  }
}

abstract class CommutativeMonoid {

  type In <: SymmetricStructure

  type M <: In#On#On#Objects

  val unit: In#On#On#C[In#On#I, M]

  val multiplication: In#On#On#C[In#On# ⊗[M, M], M]
}

object CommutativeMonoid {

  type In[SMC <: SymmetricStructure] =
    CommutativeMonoid { type In = SMC }

  final
  class coproduct[SMC <: SymmetricStructure, X <: In[SMC], Y <: In[SMC]] extends CommutativeMonoid {

    type In = SMC
    type M = SMC#On# ⊗[X#M, Y#M]

    val unit =
      scala.Predef.???
      // lunit_inv >-> x.unit ⊗ y.unit

    val multiplication =
      scala.Predef.???
      // reassoc >-> swap >-> x.multiplication ⊗ y.multiplication
  }
}
