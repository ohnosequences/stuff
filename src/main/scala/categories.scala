package ohnosequences.stuff

trait AnyCategory {

  type Objects
  type Morphisms <: AnyMorphism { type Bound = Objects }

  implicit val me: this.type = this

  type C[X <: Objects, Y <: Objects] <: Morphisms { type Source = X; type Target = Y }

  def id[X <: Objects]: C[X,X]

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z]
}

trait AnyMorphism extends Any {

  type Bound

  type Source <: Bound
  type Target <: Bound
}

case object AnyMorphism {

  type is[G <: AnyMorphism] = G with AnyMorphism {

    type Bound = G#Bound

    type Source = G#Source
    type Target = G#Target
  }
}

case object AnyCategory {

  type is[Cat <: AnyCategory] = Cat with AnyCategory {

    type Objects = Cat#Objects
    type Morphisms = Cat#Morphisms;
    type C[X <: Cat#Objects, Y <: Cat#Objects] = Cat#C[X,Y]
  }

  final implicit class MorphismsSyntax [
    cat <: AnyCategory,
    Y <: cat#Objects,
    Z <: cat#Objects
  ](
    val g: cat#C[Y,Z]
  )
  extends AnyVal {
    
    final def ∘[X <: cat#Objects](f: cat#C[X,Y])(implicit c: AnyCategory.is[cat]): cat#C[X,Z] =
      c.compose(g,f)

    final def >=>[W <: cat#Objects](h: cat#C[Z,W])(implicit c: AnyCategory.is[cat]): cat#C[Y,W] =
      c.compose(h,g)
  }
}
