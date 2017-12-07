package ohnosequences.stuff

abstract class Coproduct {

  type On <: Category
  val on: Category.is[On]

  @infix
  type +[X <: On#Objects, Y <: On#Objects] <: On#Objects

  type ∅ <: On#Objects

  def either[
      A <: On#Objects,
      B <: On#Objects,
      X <: On#Objects,
  ]
  // TODO find a convenient aliasing convention for both +'s
    : (On#C[A, X] × On#C[B, X]) -> On#C[A + B, X]

  def intro[X <: On#Objects]: On#C[∅, X]

  def left[A <: On#Objects, B <: On#Objects]: On#C[A, A + B]
  def right[A <: On#Objects, B <: On#Objects]: On#C[B, A + B]
}

object Coproduct {

  type is[S <: Coproduct] =
    S {
      type On = S#On
      // format: off
    type +[X <: On#Objects, Y <: On#Objects] = S# +[X, Y]
    type ∅                                   = S# ∅
    // format: on
    }

  def monoidalCategory[S <: Coproduct](s: S)(implicit ev: s.type <:< is[S])
    : MonoidalCategory.is[CocartesianMonoidalCategory[S]] =
    new CocartesianMonoidalCategory(ev(s))
      .asInstanceOf[MonoidalCategory.is[CocartesianMonoidalCategory[S]]]

  @inline final def apply[
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
      // format: off
      S # +[X, Y]
      // format: on

    type ∅ =
      // format: off
      S # ∅
      // format: on

    // implicits
    /////////////////////////////////////////////////////////////////////////
    @inline implicit final val _on: Category.is[S#On] =
      s.on

    @inline implicit final val _s: is[S] =
      s

    @inline implicit final def categoryMorphismSyntax[X <: Objects,
                                                      Y <: Objects](
        f: X >=> Y): Category.MorphismSyntax[S#On, X, Y] =
      new Category.MorphismSyntax(f)

    @inline implicit final def sumMorphismSyntax[X <: Objects, Y <: Objects](
        f: X >=> Y): SumMorphismSyntax[S, X, Y] =
      new SumMorphismSyntax(f)

    // aliases
    /////////////////////////////////////////////////////////////////////////
    @inline final def id[X <: Objects]: X >=> X =
      s.on.identity

    @inline final def left[A <: Objects, B <: Objects]: A >=> (A + B) =
      s left

    @inline final def right[A <: Objects, B <: Objects]: B >=> (A + B) =
      s right

    @inline final def intro[A <: Objects]: ∅ >=> A =
      s intro

    @inline final def any[Z <: Objects]: (Z + Z) >=> Z =
      id | id

    @inline final def ∇[Z <: Objects]: (Z + Z) >=> Z =
      any

    @inline final def swap[A <: Objects, B <: Objects]: (A + B) >=> (B + A) =
      right | left

    @inline final def components[
        A <: Objects,
        B <: Objects,
        X <: Objects,
    ]: ((A + B) >=> X) -> ((A >=> X) × (B >=> X)) =
      λ { f =>
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
      extends scala.AnyVal {

    @inline final def |[Z <: S#On#Objects](g: S#On#C[Z, Y])(
        implicit sum: is[S]
    ) // format: off
    : S#On#C[S# +[X, Z], Y] = // format: on
    sum either (f and g)

    @inline final def +[U <: S#On#Objects, V <: S#On#Objects](g: S#On#C[U, V])(
        implicit sum: is[S] // format: off
    )
    : S#On#C[S# +[X, U], S# +[Y, V]] = // format: on
    Category(sum.on) ⊢ { sum either (f >=> sum.left[Y, V] and g >=> sum.right) }
  }
}
////////////////////////////////////////////////////////////////////////////

final class CocartesianMonoidalCategory[S <: Coproduct](
    val coproduct: Coproduct.is[S])
    extends MonoidalCategory {

  type On = S#On
  val on = coproduct.on

  // format: off
  @infix
  type ⊗[X <: On#Objects, Y <: On#Objects] = S# +[X, Y]
  type I                                   = S# ∅
  // format: on

  def ⊗[
      A <: On#Objects,
      B <: On#Objects,
      C <: On#Objects,
      D <: On#Objects
  ]: On#C[A, B] × On#C[C, D] -> On#C[A ⊗ C, B ⊗ D] =
    λ { fg =>
      Coproduct(coproduct) ⊢ { fg.left + fg.right }
    }

  def assoc_right[
      A <: On#Objects,
      B <: On#Objects,
      C <: On#Objects
  ]: On#C[(A ⊗ B) ⊗ C, A ⊗ (B ⊗ C)] =
    Coproduct(coproduct) ⊢ {
      left |
        left >=> right[A, B + C] |
        right >=> right
    }

  def assoc_left[
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

  def unitl[A <: On#Objects]: On#C[I ⊗ A, A] =
    Coproduct(coproduct) ⊢ (intro + id[A] >=> any)

  def unitr[A <: On#Objects]: On#C[A ⊗ I, A] =
    Coproduct(coproduct) ⊢ (id[A] + intro[A] >=> any)
}
