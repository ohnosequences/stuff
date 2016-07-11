package ohnosequences.stuff.test

import org.scalatest.FunSuite
import ohnosequences.stuff._

trait AnySpan {

  type Struct <: AnyCartesianMonoidalStructure with AnySymmetricMonoidalStructure
  val  struct: Struct

  type Cat = Struct#On
  val  cat: Cat = struct.on

  type Source <: Cat#Objects
  type Target <: Cat#Objects

  // type Top = Struct# ×[Source, Target]

  val source: Cat#C[Struct# ×[Source, Target], Source]
  val target: Cat#C[Struct# ×[Source, Target], Target]
}

case class Span[
  CS <: AnyCartesianMonoidalStructure,
  Source_ <: CS#On#Objects,
  Target_ <: CS#On#Objects
](val struct: CS)(
  val source: CS#On#C[CS# ×[Source_, Target_], Source_],
  val target: CS#On#C[CS# ×[Source_, Target_], Target_]
) extends AnySpan {

  type Struct = CS

  type Source = Source_
  type Target = Target_
}


trait AnySpanCat extends AnyCategory { spn =>

  type Struct <: AnyCartesianMonoidalStructure with AnySymmetricMonoidalStructure
  val  struct: Struct
  lazy val struct_ = AnyMonoidalStructure.is(struct)

  type Cat = Struct#On
  val  cat: Cat = struct.on

  type Objects = Cat#Objects

  type C[X, Y] = AnySpan {
    type Struct = spn.Struct
    type Source = X
    type Target = Y
  }


  def id[X <: Cat#Objects]: C[X,X] = Span(struct)(struct_.left[X, X], struct_.right[X, X])

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] = (g, f) => {
    // TODO: need pullbacks on Cat
    ???
  }
}

case object AnySpanCat {

  def dagger[Spn <: AnySpanCat](spn: Spn) = DaggerCategory(spn, SpanDaggerFunctor(spn))
}

case class SpanCat[Cat_ <: AnyCategory](val cat: Cat_)
extends AnySpanCat { type Cat = Cat_ }


case class SpanDaggerFunctor[Spn <: AnySpanCat](val spn: Spn)
extends DaggerFunctor[Spn](spn) {
  type Struct = Spn#Struct
  val  struct = spn.struct
  val  struct_ = spn.struct_

  val spn_ = AnyCategory.is(spn)

  def apply[X <: Spn#Objects, Y <: Spn#Objects](f: Spn#C[X,Y]): Spn#C[Y,X] =
    Span(struct)(
      spn.compose(
        f.target: Struct#On#C[Struct# ×[X,Y], Y],
        spn.struct.symmetry[Y, X]
      ),
      spn.compose(
        f.source: Struct#On#C[Struct# ×[X,Y], X],
        spn.struct.symmetry[Y, X]
      )
    )
}
