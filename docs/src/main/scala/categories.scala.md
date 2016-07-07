
```scala
package ohnosequences.stuff

trait AnyCategory {

  type Objects
  type C[X <: Objects, Y <: Objects]

  def id[X <: Objects]: C[X,X]

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z]

  implicit val me: this.type = this
}

case object AnyCategory {

  def is[Cat <: AnyCategory](cat: Cat): is[Cat] =
    cat.asInstanceOf[is[Cat]]

  type is[Cat <: AnyCategory] = Cat {

    type Objects = Cat#Objects
    type C[X <: Cat#Objects, Y <: Cat#Objects] = Cat#C[X,Y]
  }
}

case class MorphismsSyntax [
  Cat <: AnyCategory,
  Y <: Cat#Objects,
  Z <: Cat#Objects
](
  val g: Cat#C[Y,Z]
)
extends AnyVal {

  def >=>[W <: Cat#Objects](h: Cat#C[Z,W])(implicit cat: Cat): Cat#C[Y,W] =
    AnyCategory.is(cat).compose(h,g)

  def ⊗[
    C <: Cat#Objects,
    D <: Cat#Objects,
    MCat <: AnyMonoidalStructure { type On = Cat }
  ]
  (f: Cat#C[C,D])(implicit mcat: MCat): Cat#C[ MCat# ⊗[Y,C], MCat# ⊗[Z,D]] =
    AnyMonoidalStructure.is(mcat).⊗(g,f)

  def ×[
  C <: Cat#Objects,
  D <: Cat#Objects,
  CMCat <: AnyCartesianMonoidalStructure { type On = Cat }
]
(f: Cat#C[C,D])(implicit cmcat: CMCat): Cat#C[ CMCat# ⊗[Y,C], CMCat# ⊗[Z,D]] =
  AnyMonoidalStructure.is(cmcat).×(g,f)
}

case class CategorySyntax[Cat <: AnyCategory](val cat: Cat) extends AnyVal {

  def Id: IdentityFunctor[Cat] =
    IdentityFunctor[Cat](cat)
}

```




[test/scala/categories.scala]: ../../test/scala/categories.scala.md
[main/scala/monoidalCategories.scala]: monoidalCategories.scala.md
[main/scala/distributiveLaws.scala]: distributiveLaws.scala.md
[main/scala/package.scala]: package.scala.md
[main/scala/monads.scala]: monads.scala.md
[main/scala/monoidalFunctors.scala]: monoidalFunctors.scala.md
[main/scala/functors.scala]: functors.scala.md
[main/scala/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/kleisli.scala]: kleisli.scala.md
[main/scala/categories.scala]: categories.scala.md