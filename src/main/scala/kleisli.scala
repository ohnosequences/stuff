package ohnosequences.stuff


trait AnyKleisliCategory extends AnyCategory {

  type Monad <: AnyMonad
  val monad: Monad
  type Cat = monad.On
  val cat: AnyCategory.is[Cat] = monad.on

  type M[X <: Objects] = monad.Functor#F[X]

  type Objects = monad.On#Objects // <: monad.On#Objects
  type C[X <: Objects, Y <: Objects] = Cat#C[X, monad.Functor#F[Y]]

  final def compose[X <: Objects, Y <: Objects, Z <: Objects]: (C[Y,Z], C[X,Y]) => C[X,Z] = (g,f) => {

    // TODO proper syntax
    import AnyCategory._
    implicit val c = cat

    (f:Cat#C[X,M[Y]]) >=> monad.functor(g) >=> monad.μ.at
  }

  final def id[X <: Objects]: C[X,X] = monad.η[X]
}

case class KleisliCategory[Monad0 <: AnyMonad](val monad: Monad0) extends AnyKleisliCategory {

  type Monad = Monad0
}

trait AnyKleisliFunctor extends AnyFunctor {

  type Source = target.Cat
  lazy val source: Source = target.cat
  type Target <: AnyKleisliCategory
  val target: Target

  type F[X <: Source#Objects] = X

  def apply[X <: Source#Objects, Y <: Source#Objects](f: Source#C[X,Y]): Target#C[F[X],F[Y]] = {

    // TODO proper syntax
    import AnyCategory._
    implicit val c = target.cat

    f >=> target.monad.η[Y]
  }
}

case class kleisliFunctor[KC <: AnyKleisliCategory](val target: KC) extends AnyKleisliFunctor {

  type Target = KC
}
