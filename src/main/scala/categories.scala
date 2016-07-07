package ohnosequences.stuff

trait AnyCategory {

  type Objects
  type C[X <: Objects, Y <: Objects]

  def id[X <: Objects]: C[X,X]

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z]

  implicit val me: this.type = this
}

case object AnyCategory {

  def is[Cat <: AnyCategory](cat: Cat): AnyCategory.is[Cat] = cat.asInstanceOf[AnyCategory.is[Cat]]

  type is[Cat <: AnyCategory] = Cat {

    type Objects = Cat#Objects
    type C[X <: Cat#Objects, Y <: Cat#Objects] = Cat#C[X,Y]
  }

  implicit class MorphismsSyntax [
    cat <: AnyCategory,
    Y <: cat#Objects,
    Z <: cat#Objects
  ](
    val g: cat#C[Y,Z]
  )
  extends AnyVal {

    final def >=>[W <: cat#Objects](h: cat#C[Z,W])(implicit c: cat): cat#C[Y,W] =
      is(c).compose(h,g)
  }

  implicit class CategorySyntax[Cat <: AnyCategory](val cat: Cat) extends AnyVal {

    def Id: IdentityFunctor[Cat] = IdentityFunctor[Cat](cat)

    def kleisliCategory[
      Functor0 <: AnyFunctor { type Source = Cat; type Target = Cat },
      Monad0 <: AnyMonad { type On = Cat; type Functor = Functor0 }
    ]
    (f: Functor0, m: Monad0): KleisliCategory[Cat,Functor0,Monad0] =
      KleisliCategory(m)
  }
}
