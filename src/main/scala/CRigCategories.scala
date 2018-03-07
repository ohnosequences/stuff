package ohnosequences.stuff

abstract class CRigCategory {

  type Cat <: Category
  val cat: Category.is[Cat]

  type Plus <: MonoidalCategory { type On      = Cat }
  val plus: MonoidalCategory.is[Plus { type On = Cat }]

  type PlusSymm <: SymmetricStructure { type On = Plus }
  val plusSymm: SymmetricStructure.is[PlusSymm]

  type Times <: MonoidalCategory { type On       = Cat }
  val times: MonoidalCategory.is[Times { type On = Cat }]

  type TimesSymm <: SymmetricStructure { type On = Times }
  val timesSymm: SymmetricStructure.is[TimesSymm]

  final type O =
    Plus#I

  final type ⊕[A <: Cat#Objects, B <: Cat#Objects] =
    Times# ⊗[A, B]

  final type I =
    Times#I

  final type ⊗[A <: Cat#Objects, B <: Cat#Objects] =
    Times# ⊗[A, B]

  def pack[
      A <: Cat#Objects,
      X <: Cat#Objects,
      Y <: Cat#Objects,
  ]: Cat#C[(A ⊗ X) ⊕ (A ⊗ Y), A ⊗ (X ⊕ Y)]

  def expand[
      A <: Cat#Objects,
      X <: Cat#Objects,
      Y <: Cat#Objects,
  ]: Cat#C[A ⊗ (X ⊕ Y), (A ⊗ X) ⊕ (A ⊗ Y)]
}
