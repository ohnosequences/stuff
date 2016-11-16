
```scala
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

```




[test/scala/categories.scala]: ../../test/scala/categories.scala.md
[main/scala/monoidalCategories.scala]: monoidalCategories.scala.md
[main/scala/distributiveLaws.scala]: distributiveLaws.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/monads.scala]: monads.scala.md
[main/scala/syntax/package.scala]: syntax/package.scala.md
[main/scala/syntax/functors.scala]: syntax/functors.scala.md
[main/scala/syntax/categories.scala]: syntax/categories.scala.md
[main/scala/monoidalFunctors.scala]: monoidalFunctors.scala.md
[main/scala/kleisliCoproducts.scala]: kleisliCoproducts.scala.md
[main/scala/functors.scala]: functors.scala.md
[main/scala/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/kleisli.scala]: kleisli.scala.md
[main/scala/categories.scala]: categories.scala.md