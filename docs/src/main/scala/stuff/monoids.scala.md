
```scala
package ohnosequences.stuff

abstract class Monoid {

  type In <: MonoidalCategory

  type M <: In#On#Objects

  val unit: In#On#C[In#I, M]

  val multiplication: In#On#C[In# ⊗[M, M], M]
}

object Monoid {

  type In[MC <: MonoidalCategory] =
    Monoid { type In = MC }

  final
  class UnitMonoid[MCat <: MonoidalCategory](mcat: MCat) extends Monoid {

    type In = MCat
    type M = MCat#I

    val unit =
      Category.is(MonoidalCategory.is(mcat).on).identity

    val multiplication =
      MonoidalCategory.is(mcat).unitl
  }
}

```




[test/scala/tuples/stdComparison.scala]: ../../../test/scala/tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: ../../../test/scala/tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: ../../../test/scala/functors/functorExamples.scala.md
[test/scala/sums.scala]: ../../../test/scala/sums.scala.md
[test/scala/ScalaCategory.scala]: ../../../test/scala/ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: ../../../test/scala/functions/syntax.scala.md
[test/scala/categories.scala]: ../../../test/scala/categories.scala.md
[main/scala/stuff/monoidalCategories.scala]: monoidalCategories.scala.md
[main/scala/stuff/products.scala]: products.scala.md
[main/scala/stuff/Scala.scala]: Scala.scala.md
[main/scala/stuff/package.scala]: package.scala.md
[main/scala/stuff/sums.scala]: sums.scala.md
[main/scala/stuff/monoids.scala]: monoids.scala.md
[main/scala/stuff/maybe.scala]: maybe.scala.md
[main/scala/stuff/boolean.scala]: boolean.scala.md
[main/scala/stuff/functors.scala]: functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: categories.scala.md
[main/scala/stuff/functions.scala]: functions.scala.md