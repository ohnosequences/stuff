package ohnosequences.stuff

trait AnyMonoidalCategory {

  type On <: AnyCategory
  implicit val on: On

  type ⊗[X <: On#Objects, Y <: On#Objects] <: On#Objects

  type I <: On#Objects

  def ⊗[A <: On#Objects, B <: On#Objects, C <: On#Objects, D <: On#Objects](f: On#C[A,B], g: On#C[C,D]): On#C[A ⊗ C, B ⊗ D]

  def assoc_right[A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ (A ⊗ B) ⊗ C, A ⊗ (B ⊗ C) ]
  def  assoc_left[A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ A ⊗ (B ⊗ C), (A ⊗ B) ⊗ C ]

  implicit val me: this.type = this
}

case object AnyMonoidalCategory {

  def is[MCat <: AnyMonoidalCategory](mcat: MCat): is[MCat] =
    mcat.asInstanceOf[is[MCat]]

  type is[MCat <: AnyMonoidalCategory] = MCat {

    type On = MCat#On
    type I = MCat#I

    type ⊗[X <: On#Objects, Y <: On#Objects] = MCat# ⊗[X,Y]
  }
}

trait AnyProducts extends AnyMonoidalCategory {

  type ×[A <: On#Objects, B <: On#Objects] = A ⊗ B

  def  left[A <: On#Objects, B <: On#Objects]: On#C[ A × B, A ]
  def right[A <: On#Objects, B <: On#Objects]: On#C[ A × B, B ]

  def erase[A <: On#Objects]: On#C[A, I]

  def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]
  : (On#C[X,A], On#C[X,B]) => On#C[ X, A × B ]

  def univ_inv[A <: On#Objects, B <: On#Objects, X <: On#Objects]: On#C[ X, A × B ] => (On#C[X,A], On#C[X,B]) =
    { f => (f >=> left, f >=> right) }

  def duplicate[A <: On#Objects]: On#C[ A, A × A ] = univ(AnyCategory.is(on).id, AnyCategory.is(on).id)

  def ⊗[
    A <: On#Objects, B <: On#Objects,
    C <: On#Objects, D <: On#Objects
  ](f: On#C[A,B],
    g: On#C[C,D]
  ): On#C[A × C, B × D] =
    univ(
      left  >=> f,
      right >=> g
    )

  def ×[
    A <: On#Objects, B <: On#Objects,
    C <: On#Objects, D <: On#Objects
  ](f: On#C[A,B],
    g: On#C[C,D]
  ): On#C[A × C, B × D] = ⊗(f,g)
}

trait AnyCoproducts extends AnyMonoidalCategory {

  type +[A <: On#Objects, B <: On#Objects] = A ⊗ B

  def  left[A <: On#Objects, B <: On#Objects]: On#C[ A, A + B ]
  def right[A <: On#Objects, B <: On#Objects]: On#C[ B, A + B ]

  def nothing[A <: On#Objects]: On#C[I, A]

  def univ[A <: On#Objects, B <: On#Objects, X <: On#Objects]: (On#C[A,X], On#C[B,X]) => On#C[ A + B, X ]

  def univ_inv[A <: On#Objects, B <: On#Objects, X <: On#Objects]: On#C[ A + B, X ] => (On#C[A,X], On#C[B,X]) =
    { f => (left >=> f, right >=> f) }

  def ⊗[
    A <: On#Objects, B <: On#Objects,
    C <: On#Objects, D <: On#Objects
  ](f: On#C[A,B],
    g: On#C[C,D]
  ): On#C[A + C, B + D] =
    univ(
      f >=> left,
      g >=> right
    )

  def +[
    A <: On#Objects, B <: On#Objects,
    C <: On#Objects, D <: On#Objects
  ](f: On#C[A,B],
    g: On#C[C,D]
  ): On#C[A + C, B + D] = ⊗(f, g)
}

case object AnyCoproducts {

  type is[S <: AnyCoproducts] = S {

    type On = S#On
    type I = S#I

    type ⊗[X <: On#Objects, Y <: On#Objects] = S# ⊗[X,Y]
    type ×[X <: On#Objects, Y <: On#Objects] = S# +[X,Y]
  }

  def is[MCat <: AnyCoproducts](mcat: MCat): is[MCat] =
    mcat.asInstanceOf[is[MCat]]
}

trait AnyRigStructure { rig =>

  type On <: AnyCategory

  type Plus <: AnyMonoidalCategory { type On = rig.On }
  type Mult <: AnyMonoidalCategory { type On = rig.On }

  type ⊕[X <: On#Objects, Y <: On#Objects] = Plus# ⊗[X,Y]
  type ⊗[X <: On#Objects, Y <: On#Objects] = Mult# ⊗[X,Y]

  def distribute[A <: On#Objects, B <: On#Objects, C <: On#Objects]: On#C[ (A ⊕ B) ⊗ C, (A ⊗ C) ⊕ (B ⊗ C) ]
}
