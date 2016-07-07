package ohnosequences.stuff

trait AnyMonoidalStructure {

  type On <: AnyCategory
  val on: On

  type ⊗ [X <: On#Objects, Y <: On#Objects] <: On#Objects

  type I <: On#Objects

  def ⊗[A <: On#Objects, B <: On#Objects, C <: On#Objects, D <: On#Objects](f: On#C[A,B], g: On#C[C,D]): On#C[A ⊗ C, B ⊗ D]

  def assoc_right [A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ (A ⊗ B) ⊗ C, A ⊗ (B ⊗ C) ]
  def assoc_left  [A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ A ⊗ (B ⊗ C), (A ⊗ B) ⊗ C ]

  implicit val me: this.type = this
}

case object AnyMonoidalStructure {

  def is[MCat <: AnyMonoidalStructure](mcat: MCat): is[MCat] =
    mcat.asInstanceOf[is[MCat]]

  type is[MCat <: AnyMonoidalStructure] = MCat with AnyMonoidalStructure {

    type On = MCat#On
    type I = MCat#I

    type ⊗[X <: On#Objects, Y <: On#Objects] = MCat# ⊗[X,Y]
  }

  implicit final class MonoidalStructureSyntax[
    Cat <: AnyCategory,
    A <: Cat#Objects, B <: Cat#Objects
  ]
  (val f: Cat#C[A,B]) extends AnyVal {

    def ⊗[
      C <: Cat#Objects,
      D <: Cat#Objects,
      MCat <: AnyMonoidalStructure { type On = Cat }
    ]
    (g: Cat#C[C,D])(implicit mcat: MCat): Cat#C[ MCat# ⊗[A,C], MCat# ⊗[B,D]] =
      is(mcat).⊗(f,g)
  }
}
