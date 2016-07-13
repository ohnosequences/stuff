package ohnosequences.stuff.test

import org.scalatest.FunSuite
import ohnosequences.stuff._

// trait AnySpan {
//
//   type Struct <: AnyCartesianMonoidalStructure with AnySymmetricMonoidalStructure
//   val  struct: Struct
//
//   type Cat = Struct#On
//   val  cat: Cat = struct.on
//
//   type Source <: Cat#Objects
//   type Target <: Cat#Objects
//
//   // type Top = Struct# ×[Source, Target]
//
//   val source: Cat#C[Struct# ×[Source, Target], Source]
//   val target: Cat#C[Struct# ×[Source, Target], Target]
// }
//
// case class Span[
//   CS <: AnyCartesianMonoidalStructure,
//   Source_ <: CS#On#Objects,
//   Target_ <: CS#On#Objects
// ](val struct: CS)(
//   val source: CS#On#C[CS# ×[Source_, Target_], Source_],
//   val target: CS#On#C[CS# ×[Source_, Target_], Target_]
// ) extends AnySpan {
//
//   type Struct = CS
//
//   type Source = Source_
//   type Target = Target_
// }
//
//
// trait AnySpanCat extends AnyCategory { spn =>
//
//   type Struct <: AnyCartesianMonoidalStructure with AnySymmetricMonoidalStructure
//   val  struct: Struct
//   lazy val struct_ = AnyMonoidalStructure.is(struct)
//
//   type × [A <: Struct#On#Objects, B <: Struct#On#Objects] = Struct# ×[A, B]
//
//   type Cat = Struct#On
//   val  cat: Cat = struct.on
//
//   type Objects = Cat#Objects
//
//   type C[S <: Cat#Objects, T <: Cat#Objects] = (
//     Cat#C[S × T, S], // source
//     Cat#C[S × T, T]  // target
//   )
//
//
//   def id[X <: Cat#Objects]: C[X,X] = (struct_.left[X, X], struct_.right[X, X])
//
//   def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] = (g, f) => {
//     // TODO: need pullbacks on Cat
//     ???
//   }
// }
//
// case class SpanCat[S <: AnyCartesianMonoidalStructure with AnySymmetricMonoidalStructure](val struct: S)
// extends AnySpanCat { type Struct = S }
//
// // case object AnySpanCat {
// //
// //   def dagger[Spn <: AnySpanCat](spn: Spn) = DaggerCategory(spn, SpanDaggerFunctor(spn))
// // }
//
//
// case class SpanDaggerFunctor[Spn <: AnySpanCat](val spn: Spn)
// extends DaggerFunctor[Spn](spn) {
//   // type Struct = Spn#Struct
//   // val  struct = spn.struct
//   // val  struct_ = spn.struct_
//
//   // val spn_ = AnyCategory.is(spn)
//
//   def apply[X <: Spn#Cat#Objects, Y <: Spn#Cat#Objects](f: Spn#C[X,Y]): Spn#C[Y,X] = {
//     val (fsource, ftarget): (spn.cat.C[spn.×[X,Y], X], spn.cat.C[spn.×[X,Y], Y]) = f
//
//     val src: Spn#Cat#C[Spn# ×[Y,X], Y] =
//       spn.cat.compose(
//         ftarget,
//         spn.struct.symmetry[Y,X]
//       )
//
//     val tgt: Spn#Cat#C[Spn# ×[Y,X], X] =
//       spn.cat.compose(
//         fsource,
//         spn.struct.symmetry[Y,X]
//       )
//
//     (src, tgt)
//   }
// }
