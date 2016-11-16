package ohnosequences.stuff

trait AnyCategory {

  type Objects
  type C[X <: Objects, Y <: Objects]

  def id[X <: Objects]: C[X,X]

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z]

  implicit val me: this.type = this
}

case class OppositeCategory[Category <: AnyCategory](val category: Category) {

  type Objects = Category#Objects
  type C[X <: Objects, Y <: Objects] = Category#C[Y,X]

  def id[X <: Objects]: C[X,X] =
    AnyCategory.is(category).id

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] =
    { (g,f) => AnyCategory.is(category).compose(f,g) }
}

case object AnyCategory {

  def is[Category <: AnyCategory](category: Category): is[Category] =
    category.asInstanceOf[is[Category]]

  type is[Category <: AnyCategory] = Category {

    type Objects = Category#Objects
    type C[X <: Category#Objects, Y <: Category#Objects] = Category#C[X,Y]
  }
}
