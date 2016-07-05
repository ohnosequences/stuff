package ohnosequences.stuff

trait AnyLaxMonoidalFunctor extends AnyFunctor {

  type TargetM <: AnyMonoidalStructure
  val sourceM: SourceM
  type SourceM <: AnyMonoidalStructure
  val targetM: TargetM

  // NOTE notation
  type □[X <: Source#Objects, Y <: Source#Objects] = SourceM# ⊗[X,Y]
  type ⋄[X <: Target#Objects, Y <: Target#Objects] = TargetM# ⊗[X,Y]

  type Source = SourceM#On
  lazy val source = sourceM.on
  type Target = TargetM#On
  lazy val target = targetM.on

  type Functor <: AnyFunctor {
    type Source = SourceM#On
    type Target = TargetM#On
  }
  val functor: Functor

  type F[X <: Source#Objects] = Functor#F[X]

  def apply[A <: Source#Objects, B <: Source#Objects](f: Source#C[A,B]): Target#C[F[A], F[B]] =
    AnyFunctor.is(functor)(f)

  def zip[A <: Source#Objects, B <: Source#Objects]: Target#C[F[A] ⋄ F[B], F[A □ B]]
  def unit: Target#C[TargetM#I, F[SourceM#I]]
}
