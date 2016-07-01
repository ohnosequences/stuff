package ohnosequences.stuff

trait AnyCategory {

  type Objects
  // type Morphisms <: AnyMorphism { type Category = me.type }

  implicit val me: this.type = this

  type C[X <: Objects, Y <: Objects] // <: Morphisms { type Source = X; type Target = Y }

  def id[X <: Objects]: C[X,X]

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z]

  implicit final class MorphismsSyntax[Y <: Objects, Z <: Objects](val g: C[Y,Z]) {

    final def >=>[W <: Objects](h: C[Z,W]): C[Y,W] =
      compose(h,g)
  }
}
//
// trait AnyMorphism extends Any {
//
//   // type Bound
//
//   type Category <: AnyCategory
//
//   type Source <: Category#Objects
//   type Target <: Category#Objects
//
//   // def >=>[W <: Category#Objects](h: Category#C[Target,W])(implicit c: AnyCategory.is[Category]): Category#C[Source,W] =
//   //   c.compose(h,this: Category#C[Source,Target])
// }

case object AnyMorphism {

  // type is[G <: AnyMorphism] = G with AnyMorphism {
  //
  //   type Bound = G#Bound
  //
  //   type Source = G#Source
  //   type Target = G#Target
  // }
}

case object AnyCategory {

  // type is[Cat <: AnyCategory] = Cat with AnyCategory {
  //
  //   type Objects = Cat#Objects
  //   type Morphisms = Cat#Morphisms;
  //   type C[X <: Cat#Objects, Y <: Cat#Objects] = Cat#C[X,Y]
  // }

  // implicit final class MorphismsSyntax [
  //   cat <: AnyCategory,
  //   Y <: cat#Objects,
  //   Z <: cat#Objects
  // ](
  //   val g: cat#C[Y,Z]
  // )
  // extends AnyVal {
  //
  //   // final def ∘[X <: cat#Objects](f: cat#C[X,Y])(implicit c: cat): cat#C[X,Z] =
  //   //   c.compose[X,Y,Z](g,f)
  //   //
  //   final def >=>[W <: cat#Objects](h: cat#C[Z,W])(implicit c: AnyCategory.is[cat]): cat#C[Y,W] =
  //     c.compose(h,g)
  // }
}
