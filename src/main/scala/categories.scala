package ohnosequences.stuff

abstract class AnyCategory {

  type Objects
  type C[X <: Objects, Y <: Objects]

  def id[X <: Objects]: C[X,X]

  // TODO change this: => should be ->
  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z]

  implicit val me: this.type = this
}

case object AnyCategory {

  type Opposite[Cat <: AnyCategory] = AnyCategory {
    type Objects = Cat#Objects
    type C[X <: Objects, Y <: Objects] = Cat#C[Y,X]
  }

  import scala._, Product._

  type Product[LeftC <: AnyCategory, RightC <: AnyCategory] = AnyCategory {
    type Objects = AnyProduct { type Left <: LeftC#Objects; type Right <: RightC#Objects }
    type C[X <: Objects, Y <: Objects] = LeftC#C[X#Left,Y#Left] × RightC#C[X#Right, Y#Right]
  }

  def product[LeftC <: AnyCategory, RightC <: AnyCategory](leftC: LeftC, rightC: RightC): Product[LeftC,RightC] =
    new AnyCategory {
      type Objects = AnyProduct { type Left <: LeftC#Objects; type Right <: RightC#Objects }
      type C[X <: Objects, Y <: Objects] = LeftC#C[X#Left,Y#Left] × RightC#C[X#Right, Y#Right]
      def id[X <: Objects]: C[X,X] =
        is(leftC).id × is(rightC).id

      def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] =
        (g,f) => is(leftC).compose(left(g),left(f)) × is(rightC).compose(right(g),right(f))
    }

  def opposite[Category <: AnyCategory](category: Category): Opposite[Category] =
    new AnyCategory {

      type Objects = Category#Objects
      type C[X <: Objects, Y <: Objects] = Category#C[Y,X]

      def id[X <: Objects]: C[X,X] =
        AnyCategory.is(category).id

      def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] =
        { (g,f) => AnyCategory.is(category).compose(f,g) }
    }

  type is[Category <: AnyCategory] = Category {
    type Objects = Category#Objects
    type C[X <: Category#Objects, Y <: Category#Objects] = Category#C[X,Y]
  }

  def is[Category <: AnyCategory](category: Category): is[Category] =
    category.asInstanceOf[is[Category]]
}
