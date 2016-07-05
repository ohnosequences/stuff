package ohnosequences.stuff

trait AnyLaxMonoidalFunctor extends AnyFunctor {

  type TargetM <: AnyMonoidalStructure
  type SourceM <: AnyMonoidalStructure

  // NOTE notation
  type □[X <: Source#Objects, Y <: Source#Objects] = SourceM# ⊗[X,Y]
  type ⋄[X <: Target#Objects, Y <: Target#Objects] = TargetM# ⊗[X,Y]

  type Source = SourceM#On
  type Target = TargetM#On

  type Functor <: AnyFunctor {
    type Source = SourceM#On
    type Target = TargetM#On
  }

  type F[X <: Source#Objects] = Functor#F[X]

  def zip[A <: Source#Objects, B <: Source#Objects]: Target#C[ F[A] ⋄ F[B], F[A □ B] ]
  def unit[A <: Source#Objects]: Target#C[TargetM#I, F[SourceM#I]]
}
