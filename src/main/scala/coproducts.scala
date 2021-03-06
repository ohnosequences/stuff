package ohnosequences.stuff

abstract class Coproduct {

  type On <: Category
  val on: Category.is[On]

  @infix
  type +[X <: On#Objects, Y <: On#Objects] <: On#Objects

  type ∅ <: On#Objects

  def either[A <: On#Objects, B <: On#Objects, X <: On#Objects]
    : (On#C[A, X] × On#C[B, X]) -> On#C[A + B, X]

  def intro[X <: On#Objects]: On#C[∅, X]

  def left[A <: On#Objects, B <: On#Objects]: On#C[A, A + B]
  def right[A <: On#Objects, B <: On#Objects]: On#C[B, A + B]
}

object Coproduct {

  type is[S <: Coproduct] =
    S {
      type On = S#On

      type +[X <: On#Objects, Y <: On#Objects] = S# +[X, Y]
      type ∅                                   = S# ∅

    }

  // Cocartesian monoidal categories
  //////////////////////////////////////////////////////////////////////////////
  type CocartesianMonoidalCategory[S0 <: Coproduct] =
    CocartesianMonoidalCategoryImpl {

      type On =
        S0#On

      type ⊗[X <: On#Objects, Y <: On#Objects] =
        S0# +[X, Y]

      type I =
        S0# ∅

    }

  @inline
  final def monoidalCategory[S <: Coproduct]
    : is[S] -> MonoidalCategory.is[CocartesianMonoidalCategory[S]] = {
    coproduct: is[S] =>
      new CocartesianMonoidalCategoryImpl {

        type On =
          S#On
        @infix
        type ⊗[X <: S#On#Objects, Y <: S#On#Objects] =
          S# +[X, Y]

        type I =
          S# ∅

        val on =
          coproduct.on

        final def ⊗[
            A <: On#Objects,
            B <: On#Objects,
            C <: On#Objects,
            D <: On#Objects
        ]: On#C[A, B] × On#C[C, D] -> On#C[A ⊗ C, B ⊗ D] = { fg =>
          Coproduct(coproduct) ⊢ { fg.left + fg.right }
        }

        final def assoc_right[
            A <: On#Objects,
            B <: On#Objects,
            C <: On#Objects
        ]: On#C[(A ⊗ B) ⊗ C, A ⊗ (B ⊗ C)] =
          Coproduct(coproduct) ⊢ {
            left |
              left >=> right[A, B + C] |
              right >=> right
          }

        final def assoc_left[
            A <: On#Objects,
            B <: On#Objects,
            C <: On#Objects
        ]: On#C[A ⊗ (B ⊗ C), A ⊗ B ⊗ C] =
          Coproduct(coproduct) ⊢ {
            left >=> left[A + B, C] | {
              right >=> left[A + B, C] |
                right
            }
          }

        final def unitl[A <: On#Objects]: On#C[I ⊗ A, A] =
          Coproduct(coproduct) ⊢ (intro + id[A] >=> any)

        final def lunit[A <: On#Objects]: On#C[A, I ⊗ A] =
          coproduct.right

        final def unitr[A <: On#Objects]: On#C[A ⊗ I, A] =
          Coproduct(coproduct) ⊢ (id[A] + intro[A] >=> any)

        final def runit[A <: On#Objects]: On#C[A, A ⊗ I] =
          coproduct.left
      }
  }

  sealed abstract class CocartesianMonoidalCategoryImpl extends MonoidalCategory
  //////////////////////////////////////////////////////////////////////////////

  // symmetric monoidal structure
  //////////////////////////////////////////////////////////////////////////////
  type SymmetricMonoidalStructure[S0 <: Coproduct] =
    SymmetricMonoidalStructureImpl {

      type On =
        CocartesianMonoidalCategory[S0]
    }

  @inline
  final def symmetricMonoidalStructure[S0 <: Coproduct]
    : is[S0] -> SymmetricStructure.is[SymmetricMonoidalStructure[S0]] = {
    coproduct =>
      new SymmetricMonoidalStructureImpl {

        type On = CocartesianMonoidalCategory[S0]
        val on = monoidalCategory(coproduct)

        def swap[X <: On#On#Objects, Y <: On#On#Objects]
          : On#On#C[On# ⊗[X, Y], On# ⊗[Y, X]] =
          Coproduct(coproduct) ⊢ { right | left }
      }
  }

  sealed abstract class SymmetricMonoidalStructureImpl
      extends SymmetricStructure
  //////////////////////////////////////////////////////////////////////////////
  @inline
  final def apply[
      S <: Coproduct
  ](s: S)(implicit ev: s.type <:< is[S]): Syntax[S] =
    new Syntax(ev(s))

  final class Syntax[S <: Coproduct](val s: is[S]) {

    // type aliases
    /////////////////////////////////////////////////////////////////////////
    type Objects =
      S#On#Objects

    @infix
    type >=>[X <: S#On#Objects, Y <: S#On#Objects] =
      S#On#C[X, Y]

    @infix
    type +[X <: S#On#Objects, Y <: S#On#Objects] =
      S# +[X, Y]

    type ∅ =
      S# ∅

    // implicits
    /////////////////////////////////////////////////////////////////////////
    @inline
    implicit final val _on: Category.is[S#On] =
      s.on

    @inline
    implicit final val _s: is[S] =
      s

    @inline
    implicit final def categoryMorphismSyntax[X <: Objects, Y <: Objects](
        f: X >=> Y): Category.MorphismSyntax[S#On, X, Y] =
      new Category.MorphismSyntax(f)

    @inline
    implicit final def sumMorphismSyntax[X <: Objects, Y <: Objects](
        f: X >=> Y): SumMorphismSyntax[S, X, Y] =
      new SumMorphismSyntax(f)

    // aliases
    /////////////////////////////////////////////////////////////////////////
    @inline
    final def id[X <: Objects]: X >=> X =
      s.on.identity

    @inline
    final def left[A <: Objects, B <: Objects]: A >=> (A + B) =
      s left

    @inline
    final def right[A <: Objects, B <: Objects]: B >=> (A + B) =
      s right

    @inline
    final def intro[A <: Objects]: ∅ >=> A =
      s intro

    @inline
    final def any[Z <: Objects]: (Z + Z) >=> Z =
      id | id

    @inline
    final def ∇[Z <: Objects]: (Z + Z) >=> Z =
      any

    @inline
    final def swap[A <: Objects, B <: Objects]: (A + B) >=> (B + A) =
      right | left

    @inline
    final def components[
        A <: Objects,
        B <: Objects,
        X <: Objects,
    ]: ((A + B) >=> X) -> ((A >=> X) × (B >=> X)) = { f =>
      left >=> f and right >=> f
    }

    lazy val monCat: MonoidalCategory.is[CocartesianMonoidalCategory[S]] =
      monoidalCategory(s)

    @inline
    final def +-[A <: Objects]
      : MonoidalCategory.LeftTensor[CocartesianMonoidalCategory[S], A] =
      MonoidalCategory[CocartesianMonoidalCategory[S]](monCat) ⊢ ⊗-[A]

    @inline
    final def -+[A <: Objects]
      : MonoidalCategory.RightTensor[CocartesianMonoidalCategory[S], A] =
      MonoidalCategory[CocartesianMonoidalCategory[S]](monCat) ⊢ -⊗[A]
  }

  final class SumMorphismSyntax[
      S <: Coproduct,
      X <: S#On#Objects,
      Y <: S#On#Objects
  ](val f: S#On#C[X, Y])
      extends CompileTime {

    @inline
    final def |[Z <: S#On#Objects](g: S#On#C[Z, Y])(
        implicit sum: is[S]
    ): S#On#C[S# +[X, Z], Y] =
      sum either (f and g)

    @inline
    final def +[U <: S#On#Objects, V <: S#On#Objects](g: S#On#C[U, V])(
        implicit sum: is[S]
    ): S#On#C[S# +[X, U], S# +[Y, V]] =
      Category(sum.on) ⊢ {
        sum either (f >=> sum.left[Y, V] and g >=> sum.right)
      }
  }
}
