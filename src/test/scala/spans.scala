package ohnosequences.stuff.test

import org.scalatest.FunSuite
import ohnosequences.stuff._

trait AnySpan {

  type Cat <: AnyCategory
  val  cat: Cat

  type Source <: Cat#Objects
  type Top    <: Cat#Objects
  type Target <: Cat#Objects

  val source: Cat#C[Top, Source]
  val target: Cat#C[Top, Target]
}

case class Span[
  Cat_ <: AnyCategory,
  Source_ <: Cat_ #Objects,
  Top_    <: Cat_ #Objects,
  Target_ <: Cat_ #Objects
](val cat: Cat_)(
  val source: Cat_ #C[Top_, Source_],
  val target: Cat_ #C[Top_, Target_]
) extends AnySpan {

  type Cat = Cat_

  type Source = Source_
  type Top    = Top_
  type Target = Target_
}


trait AnySpanCat extends AnyCategory { spanCat =>

  type Cat <: AnyCategory
  val  cat: Cat
  private lazy val cat_ = AnyCategory.is(cat)

  type Objects = Cat#Objects

  type C[X, Y] <: AnySpan {
    type Cat = spanCat.Cat
    type Source = X
    type Target = Y
  }


  def id[X <: Objects]: C[X,X] = Span(cat)(cat_.id[X], cat_.id[X])

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


case class SpanDaggerFunctor[Spn <: AnySpanCat](val spanCat: Spn)
extends DaggerFunctor[Spn](spanCat) {
  val spc = AnyCategory.is(spanCat)

  def apply[X <: Spn#Objects, Y <: Spn#Objects](f: Spn#C[X,Y]): Op[Spn]#C[X,Y] = ???
    // Span(spc.op)(f.target, f.source)
}
