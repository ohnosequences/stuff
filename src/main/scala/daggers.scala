package ohnosequences.stuff


trait AnyDaggerFunctor extends AnyFunctor {

  type Cat <: AnyCategory
  val  cat: Cat

  type Source = Cat
  val  source = cat

  type Target = OppositeCategory[Cat]
  val  target = OppositeCategory(cat)

  type F[Z <: Cat#Objects] = Z

  def apply[X <: Cat#Objects, Y <: Cat#Objects](f: Cat#C[X,Y]): Cat#C[Y,X]
}

abstract class DaggerFunctor[Cat_ <: AnyCategory](val cat: Cat_)
extends AnyDaggerFunctor { type Cat = Cat_ }

case object AnyDaggerFunctor {

  type On[C <: AnyCategory] = AnyDaggerFunctor { type Cat = C }
}


trait AnyDaggerCategory extends AnyCategory { kleisli =>

  type Cat <: AnyCategory
  val  cat: Cat
  private lazy val cat_ = AnyCategory.is(cat)

  type Functor <: AnyDaggerFunctor.On[Cat]
  val  functor: Functor

  type Objects = Cat#Objects

  type C[X <: Objects, Y <: Objects] = (Cat#C[X, Y], Cat#C[Y, X])

  final def id[X <: Objects]: C[X,X] = (cat_.id[X], cat_.op.id[X])

  final def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y, Z], C[X, Y]) => C[X, Z] = {
    case (
      (g, gDag),
      (f, fDag)
    ) =>
      (
        cat_.compose(g, f),
        cat_.op.compose(gDag, fDag)
      )
  }
}

case class DaggerCategory[
  Cat_ <: AnyCategory,
  Functor_ <: AnyDaggerFunctor.On[Cat_]
](val cat: Cat_,
  val functor: Functor_
) extends AnyDaggerCategory {

  type Cat = Cat_
  type Functor = Functor_
}
