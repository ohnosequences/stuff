
```scala
package ohnosequences.stuff

trait AnyLaxMonoidalFunctor {

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

  def zip[A <: SourceCategory#Objects, B <: SourceCategory#Objects]: TargetCategory#C[Functor#F[A] ⋄ Functor#F[B], Functor#F[A □ B]]

  def unit: TargetCategory#C[TargetMonoidalCategory#I, Functor#F[SourceMonoidalCategory#I]]
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
```


A functor between cartesian monoidal categories is automatically colax monoidal. This construction only requires of the domain to be *affine* (a terminal unit), but we don't want to make this overly complex. See for example the [nLab](https://ncatlab.org/nlab/show/semicartesian+monoidal+category).


```scala
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
```


The co-dual of the above.


```scala
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