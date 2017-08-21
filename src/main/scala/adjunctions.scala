package ohnosequences.stuff

trait AnyAdjunction {

  type LeftCat  <: AnyCategory
  type RightCat <: AnyCategory

  type LeftAdjoint  <: LeftCat  ⟶ RightCat
  type RightAdjoint <: RightCat ⟶ LeftCat

  type L[X <: LeftCat#Objects]  = LeftAdjoint#F[X]
  type R[X <: RightCat#Objects] = RightAdjoint#F[X]

  def toLeft[X <: LeftCat#Objects, Y <: RightCat#Objects] : RightCat #C[L[X],Y]  => LeftCat  #C[X,R[Y]]
  def toRight[X <: LeftCat#Objects, Y <: RightCat#Objects]: LeftCat  #C[X,R[Y]]  => RightCat #C[L[X],Y]
}
