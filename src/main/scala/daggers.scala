package ohnosequences.stuff


trait AnyDaggerCategory {

  type On <: AnyCategory
  val  on: On

  /*
    The dagger "†" should of course satisfy

    1. dagger(dagger(f)) = f
    2. dagger(id) = id
    3. dagger(f >=> g) = dagger(g) >=> dagger(f)
  */
  def dagger[X <: On#Objects, Y <: On#Objects]: On#C[X,Y] => On#C[Y,X]
}

case object AnyDaggerCategory {

  def is[DC <: AnyDaggerCategory](dc: DC): is[DC] = dc.asInstanceOf[is[DC]]
  type is[DC <: AnyDaggerCategory] = DC { type On = DC#On }
}

abstract class DaggerCategory[Cat_ <: AnyCategory](val on: Cat_) extends AnyDaggerCategory {

  type On = Cat_
}

/* Example of a dagger category; we first build a category, and then a dagger on it. */
trait AnyCofreeDaggerCategory extends AnyCategory {

  type On <: AnyCategory
  val on: On

  type Objects = On#Objects
  type C[X <: Objects, Y <: Objects] = (On#C[X,Y], On#C[Y,X])

  def id[X <: Objects] = (AnyCategory.is(on).id[X], AnyCategory.is(on).id[X])

  def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] =
    { case ((f,df),(g,dg)) => ( AnyCategory.is(on).compose(f,g), AnyCategory.is(on).compose(dg,df) ) }
}

case class CofreeDaggerCategory[On_ <: AnyCategory](val on: On_) extends AnyCofreeDaggerCategory {

  type On = On_
}

/* I know, names are confusing. This is the dagger structure on that category */
case class CofreeDaggerCategoryStructure[Cat0 <: AnyCategory](val c: Cat0) extends DaggerCategory(CofreeDaggerCategory(c)) {

  def dagger[X <: On#Objects, Y <: On#Objects]: On#C[X,Y] => On#C[Y,X] =
    { case (f,df) => (df,f) }
}

/*
  This trait is just a marker for declaring that a functor is a †-functor between two †-categories. For that, it should satisfy

  1. target.dagger(functor(f)) = functor(source.dagger(f))
*/
trait AnyDaggerFunctor {

  type Source <: AnyDaggerCategory
  val source: Source

  type Target <: AnyDaggerCategory
  val target: Target

  type Functor <: Source#On ⟶ Target#On
  val functor: Functor
}

abstract class DaggerFunctor[
  Source_ <: AnyDaggerCategory,
  Functor_ <: Source_ #On ⟶ Target_ #On,
  Target_ <: AnyDaggerCategory
]
(
  val source: Source_,
  val functor: Functor_,
  val target: Target_
)
extends AnyDaggerFunctor {

  type Source = Source_
  type Target = Target_
  type Functor = Functor_
}
