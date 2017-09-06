package ohnosequences.stuff.syntax

import ohnosequences.stuff._
import ohnosequences.stuff.products._

// just a draft
object monoidalCategories {

  final
  class syntax[MonCat <: MonoidalCategory](val monCat: MonCat) extends scala.AnyVal {

    type Cat =
      MonCat#On

    type Objects =
      MonCat#On#Objects

    type C[X <: Cat#Objects, Y <: Cat#Objects] =
      Cat#C[X,Y]

    @inline
    implicit final
    def _cat: Cat =
      MonoidalCategory.is(monCat).on

    @inline
    implicit final
    def _monCat: MonCat =
      monCat

    @inline
    implicit final
    def morphismSyntax[X <: Cat#Objects, Y <: Cat#Objects](f: C[X,Y]): Category.MorphismSyntax[Cat, X, Y] =
      new Category.MorphismSyntax[Cat,X,Y](f)

    @inline
    implicit final
    def monoidalMorphismSyntax[X <: Cat#Objects, Y <: Cat#Objects](f: Cat#C[X,Y]): MorphismSyntax[MonCat, X, Y] =
      new MorphismSyntax(f)
  }

  implicit final
  class AddMonoidalCategorySyntax[MonCat <: MonoidalCategory](val monCat: MonCat) extends scala.AnyVal {

    def monoidalCategory: syntax[MonCat] =
      new syntax(monCat)
  }

  final
  class MorphismSyntax[MonCat <: MonoidalCategory, A1 <: MonCat#On#Objects, B1 <: MonCat#On#Objects](val f: MonCat#On#C[A1,B1]) extends scala.AnyVal {

    @inline
    final
    def ⊗[A2 <: MonCat#On#Objects, B2 <: MonCat#On#Objects](g: MonCat#On#C[A2,B2])(implicit monCat: MonCat): MonCat#On#C[MonCat# ⊗[A1,A2], MonCat# ⊗[B1,B2]] =
      MonoidalCategory.is(monCat). ⊗ (f and g)
  }
}
