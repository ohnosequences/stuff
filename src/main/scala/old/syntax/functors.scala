package ohnosequences.stuff

final case class FunctorSyntax[functor <: AnyFunctor](val functor: functor) extends scala.AnyVal {

  def >->[G0 <: AnyFunctor { type Source = functor#Target }](g: G0): functor >-> G0 =
    FunctorComposition(functor,g)

  def id[
    F0  >: functor         <: functor { type Source = C; type Target = D },
    C   >: functor#Source  <: functor#Source,
    D   >: functor#Target  <: functor#Target
  ]
  : IdentityNaturalTransformation[C, F0, D] =
    IdentityNaturalTransformation(functor: F0)
}
