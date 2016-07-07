package ohnosequences.stuff

trait AnyDistributiveLaw {

  type Base <: AnyCategory
  type First <: AnyMonad { type On = Base }
  type Second <: AnyMonad { type On = Base }

  type δ <: (First >=> Second) ~> (Second >=> First)
  val δ: δ
}
