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

case class MorphismsSyntax[
  Category <: AnyCategory,
  Y <: Category#Objects,
  Z <: Category#Objects
]
(val g: Category#C[Y,Z]) extends AnyVal {

  def >=>[W <: Category#Objects](h: Category#C[Z,W])(implicit category: Category): Category#C[Y,W] =
    AnyCategory.is(category).compose(h,g)

  def ⊗[
    C <: Category#Objects,
    D <: Category#Objects,
    MCat <: AnyMonoidalCategory { type On = Category }
  ]
  (f: Category#C[C,D])(implicit mcat: MCat): Category#C[ MCat# ⊗[Y,C], MCat# ⊗[Z,D]] =
    AnyMonoidalCategory.is(mcat).⊗(g,f)

  def ×[
    C <: Category#Objects,
    D <: Category#Objects,
    CMCat <: AnyProducts { type On = Category }
  ]
  (f: Category#C[C,D])(implicit cmcat: CMCat): Category#C[ CMCat# ⊗[Y,C], CMCat# ⊗[Z,D]] =
    AnyMonoidalCategory.is(cmcat).×(g,f)

  def +[
    C <: Category#Objects,
    D <: Category#Objects,
    CMCat <: AnyCoproducts { type On = Category }
  ]
  (f: Category#C[C,D])(implicit cmcat: CMCat): Category#C[ CMCat# +[Y,C], CMCat# +[Z,D]] =
    AnyMonoidalCategory.is(cmcat).+(g,f)

  def |[
    X <: Category#Objects,
    CMCat <: AnyCoproducts { type On = Category }
  ](f: Category#C[X,Z])(implicit cmcat: CMCat): Category#C[CMCat# +[Y,X], Z] =
    AnyMonoidalCategory.is(cmcat).univ(g,f)

  def &[
    W <: Category#Objects,
    CMCat <: AnyProducts { type On = Category }
  ](f: Category#C[Y,W])(implicit cmcat: CMCat): Category#C[Y, CMCat# ×[Z,W]] =
    AnyMonoidalCategory.is(cmcat).univ(g,f)
}

case class CategorySyntax[Category <: AnyCategory](val category: Category) extends AnyVal {

  def Id: IdentityFunctor[Category] =
    IdentityFunctor(category)

  def op: OppositeCategory[Category] =
    OppositeCategory(category)
}
