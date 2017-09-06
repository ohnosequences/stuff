// package ohnosequences.stuff.syntax
//
// import ohnosequences.stuff.{ Category, inline }
// import ohnosequences.stuff.products._
//
// // just a draft
// object categories {
//
//   final
//   class syntax[Cat <: Category](val cat: Cat) extends scala.AnyVal {
//
//     @inline
//     implicit final
//     def _cat: Cat =
//       cat
//
//     @inline
//     implicit final
//     def morphismSyntax[X <: Cat#Objects, Y <: Cat#Objects](f: Cat#C[X,Y]): MorphismSyntax[Cat, X, Y] =
//       new MorphismSyntax(f)
//
//     type Objects =
//       Cat#Objects
//
//     type C[X <: Cat#Objects, Y <: Cat#Objects] =
//       Cat#C[X,Y]
//
//     @inline
//     final
//     def id[X <: Cat#Objects]: C[X,X] =
//       Category.is(cat).identity[X]
//   }
//
//   implicit final
//   class AddSyntax[Cat <: Category](val cat: Cat) extends scala.AnyVal {
//
//     def category: syntax[Cat] =
//       new syntax(cat)
//   }
//
//   @inline
//   implicit final
//   def morphismSyntax[Cat <: Category, X <: Cat#Objects, Y <: Cat#Objects](f: Cat#C[X,Y]): MorphismSyntax[Cat, X, Y] =
//     new MorphismSyntax(f)
//
//   final
//   class MorphismSyntax[Cat <: Category, X <: Cat#Objects, Y <: Cat#Objects](val f: Cat#C[X,Y]) extends scala.AnyVal {
//
//     @inline
//     final
//     def >=>[Z <: Cat#Objects](g: Cat#C[Y,Z])(implicit cat: Cat): Cat#C[X,Z] =
//       Category.is(cat).composition at (f and g)
//   }
// }
