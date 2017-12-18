package ohnosequences.stuff

object KleisliCategory {

  type Of[Mnd <: Monad] =
    Impl {

      type Objects =
        Mnd#On#Source#Objects

      type C[X <: Objects, Y <: Objects] =
        Mnd#On#Source#C[X, Mnd#On#F[Y]]
    }

  @inline
  final def of[Mnd <: Monad]: Monad.is[Mnd] -> Category.is[Of[Mnd]] = { monad =>
    new Impl {

      type Objects =
        Mnd#On#Source#Objects

      type C[X <: Objects, Y <: Objects] =
        Mnd#On#Source#C[X, Mnd#On#F[Y]]

      @inline
      final def identity[X <: Objects]: C[X, X] =
        monad.ι[X]

      @inline
      final def composition[
          X <: Objects,
          Y <: Objects,
          Z <: Objects
      ]: C[X, Y] × C[Y, Z] -> C[X, Z] = { fg =>
        Category(monad.on.target) ⊢ {
          fg.left >=> monad.on(fg.right) >=> monad.μ[Z]
        }
      }
    }
  }

  sealed abstract class Impl extends Category
}
