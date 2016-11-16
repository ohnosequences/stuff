package ohnosequences.stuff

trait AnyLaxMonoidalFunctor { laxMonoidalFunctor =>

  type SourceCategory <: AnyCategory
  lazy val sourceCategory: SourceCategory = sourceMonoidalCategory.on

  type SourceMonoidalCategory <: AnyMonoidalCategory { type On = SourceCategory }
  val sourceMonoidalCategory: SourceMonoidalCategory

  type TargetCategory <: AnyCategory
  lazy val targetCategory: TargetCategory = targetMonoidalCategory.on

  type TargetMonoidalCategory <: AnyMonoidalCategory { type On = TargetCategory }
  val targetMonoidalCategory: TargetMonoidalCategory

  // NOTE notation
  type □[X <: SourceCategory#Objects, Y <: SourceCategory#Objects] = SourceMonoidalCategory# ⊗[X,Y]
  type ⋄[X <: TargetCategory#Objects, Y <: TargetCategory#Objects] = TargetMonoidalCategory# ⊗[X,Y]

  type Functor <: AnyFunctor {
    type Source = laxMonoidalFunctor.SourceCategory
    type Target = laxMonoidalFunctor.TargetCategory
  }
  val functor: Functor

  def zip[A <: SourceCategory#Objects, B <: SourceCategory#Objects]: TargetCategory#C[Functor#F[A] ⋄ Functor#F[B], Functor#F[A □ B]]

  def unit: TargetCategory#C[TargetMonoidalCategory#I, Functor#F[SourceMonoidalCategory#I]]
}

case object AnyLaxMonoidalFunctor {

  type is[LMF <: AnyLaxMonoidalFunctor] = LMF {

    type SourceCategory         = LMF#SourceCategory
    type SourceMonoidalCategory = LMF#SourceMonoidalCategory
    type Functor                = LMF#Functor
    type TargetMonoidalCategory = LMF#TargetMonoidalCategory
    type TargetCategory         = LMF#TargetCategory
  }

  def is[LMF0 <: AnyLaxMonoidalFunctor](lmf:LMF0): is[LMF0] =  lmf.asInstanceOf[is[LMF0]]
}

abstract class LaxMonoidalFunctor[
  SCat <: AnyCategory,
  SM <: AnyMonoidalCategory { type On = SCat },
  Functor0 <: AnyFunctor { type Source = SCat; type Target = TCat },
  TM <: AnyMonoidalCategory { type On = TCat },
  TCat <: AnyCategory
]
(
  val sourceMonoidalCategory: SM,
  val functor: Functor0,
  val targetMonoidalCategory: TM
)
extends AnyLaxMonoidalFunctor {

  type SourceCategory         = SCat
  type SourceMonoidalCategory = SM
  type Functor                = Functor0
  type TargetMonoidalCategory = TM
  type TargetCategory         = TCat
}

trait AnyLaxMonoidalFunctorComposition extends AnyLaxMonoidalFunctor { composition =>

  type SourceCategory <: AnyCategory
  type First <: AnyLaxMonoidalFunctor {
    type SourceCategory = composition.SourceCategory
    type Functor <: AnyFunctor {
      type Source = composition.SourceCategory
      type Target = composition.MiddleCategory
    }
    type TargetCategory = composition.MiddleCategory
  }
  val first: First

  type MiddleCategory <: AnyCategory

  type TargetCategory <: AnyCategory
  type Second <: AnyLaxMonoidalFunctor {
    type SourceCategory = composition.MiddleCategory
    type TargetCategory = composition.TargetCategory
    type SourceMonoidalCategory = First#TargetMonoidalCategory
    type Functor <: AnyFunctor {
      type Source = composition.MiddleCategory
      type Target = composition.TargetCategory
    }
  }
  val second: Second

  type SourceMonoidalCategory = First#SourceMonoidalCategory
  lazy val SourceMonoidalCategory: SourceMonoidalCategory = first.sourceMonoidalCategory

  type TargetMonoidalCategory = Second#TargetMonoidalCategory
  lazy val TargetMonoidalCategory: TargetMonoidalCategory = second.targetMonoidalCategory

  type Functor = FunctorComposition[First#Functor, Second#Functor]
  lazy val functor: Functor = (first: First).functor >=> (second: Second).functor

  def zip[A <: SourceCategory#Objects, B <: SourceCategory#Objects]: TargetCategory#C[Functor#F[A] ⋄ Functor#F[B], Functor#F[A □ B]] =
    AnyCategory.is(targetCategory).compose(
      AnyFunctor.is(AnyLaxMonoidalFunctor.is(second).functor)(AnyLaxMonoidalFunctor.is(first).zip[A,B]),
      AnyLaxMonoidalFunctor.is(second).zip[First#Functor#F[A],First#Functor#F[B]]
    )

  def unit: TargetCategory#C[TargetMonoidalCategory#I, Functor#F[SourceMonoidalCategory#I]] =
    AnyCategory.is(targetCategory).compose(
      AnyFunctor.is(AnyLaxMonoidalFunctor.is(second).functor)(AnyLaxMonoidalFunctor.is(first).unit),
      AnyLaxMonoidalFunctor.is(second).unit
    )
}


trait AnyColaxMonoidalFunctor {

  type SourceCategory <: AnyCategory
  lazy val sourceCategory: SourceCategory = sourceMonoidalCategory.on

  type SourceMonoidalCategory <: AnyMonoidalCategory { type On = SourceCategory }
  val sourceMonoidalCategory: SourceMonoidalCategory

  type TargetCategory <: AnyCategory
  lazy val targetCategory: TargetCategory = targetMonoidalCategory.on

  type TargetMonoidalCategory <: AnyMonoidalCategory { type On = TargetCategory }
  val targetMonoidalCategory: TargetMonoidalCategory

  // NOTE notation
  type □[X <: SourceCategory#Objects, Y <: SourceCategory#Objects] = SourceMonoidalCategory# ⊗[X,Y]
  type ⋄[X <: TargetCategory#Objects, Y <: TargetCategory#Objects] = TargetMonoidalCategory# ⊗[X,Y]

  type Functor <: AnyFunctor {
    type Source = SourceCategory
    type Target = TargetCategory
  }
  val functor: Functor

  def unzip[A <: SourceCategory#Objects, B <: SourceCategory#Objects]: TargetCategory#C[Functor#F[A □ B], Functor#F[A] ⋄ Functor#F[B]]

  def counit: TargetCategory#C[Functor#F[SourceMonoidalCategory#I], TargetMonoidalCategory#I]
}

abstract class ColaxMonoidalFunctor[
  SCat <: AnyCategory,
  SM <: AnyMonoidalCategory { type On = SCat },
  Functor0 <: AnyFunctor { type Source = SCat; type Target = TCat },
  TM <: AnyMonoidalCategory { type On = TCat },
  TCat <: AnyCategory
]
(
  val sourceMonoidalCategory: SM,
  val functor: Functor0,
  val targetMonoidalCategory: TM
)
extends AnyColaxMonoidalFunctor {

  type SourceCategory         = SCat
  type SourceMonoidalCategory = SM
  type Functor                = Functor0
  type TargetMonoidalCategory = TM
  type TargetCategory         = TCat
}


/*
  A functor between cartesian monoidal categories is automatically colax monoidal. This construction only requires of the domain to be *affine* (a terminal unit), but we don't want to make this overly complex. See for example the [nLab](https://ncatlab.org/nlab/show/semicartesian+monoidal+category).
*/
case class ColaxCartesianMonoidalFunctor[
  SCat <: AnyCategory,
  SM <: AnyProducts { type On = SCat },
  Functor0 <: AnyFunctor { type Source = SCat; type Target = TCat },
  TM <: AnyProducts { type On = TCat },
  TCat <: AnyCategory
](
  val sourceMonoidalCategory0: SM,
  val functor0: Functor0,
  val targetMonoidalCategory0: TM
)
extends ColaxMonoidalFunctor[SCat,SM,Functor0,TM,TCat](sourceMonoidalCategory0, functor0, targetMonoidalCategory0) {

  def unzip[A <: SourceCategory#Objects, B <: SourceCategory#Objects]: TargetCategory#C[Functor#F[A □ B], Functor#F[A] ⋄ Functor#F[B]] =
    AnyMonoidalCategory.is(targetMonoidalCategory).univ(
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceMonoidalCategory).left[A,B]),
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceMonoidalCategory).right[A,B])
    )

  def counit: TargetCategory#C[Functor#F[SourceMonoidalCategory#I], TargetMonoidalCategory#I] =
    AnyMonoidalCategory.is(targetMonoidalCategory).erase
}

/*
  The co-dual of the above.
*/
case class LaxCocartesianMonoidalFunctor[
  SCat <: AnyCategory,
  SM <: AnyCoproducts { type On = SCat },
  Functor0 <: AnyFunctor { type Source = SCat; type Target = TCat },
  TM <: AnyCoproducts { type On = TCat },
  TCat <: AnyCategory
](
  val sourceMonoidalCategory0: SM,
  val functor0: Functor0,
  val targetMonoidalCategory0: TM
)
extends LaxMonoidalFunctor[SCat,SM,Functor0,TM,TCat](sourceMonoidalCategory0, functor0, targetMonoidalCategory0) {

  def zip[A <: SourceCategory#Objects, B <: SourceCategory#Objects]: TargetCategory#C[Functor#F[A] ⋄ Functor#F[B], Functor#F[A □ B]] =
    AnyMonoidalCategory.is(targetMonoidalCategory).univ(
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceMonoidalCategory).left[A,B]),
      AnyFunctor.is(functor)(AnyMonoidalCategory.is(sourceMonoidalCategory).right[A,B])
    )

  def unit: TargetCategory#C[TargetMonoidalCategory#I, Functor#F[SourceMonoidalCategory#I]] =
    AnyMonoidalCategory.is(targetMonoidalCategory).nothing

}
