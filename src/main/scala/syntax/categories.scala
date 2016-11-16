package ohnosequences.stuff

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
