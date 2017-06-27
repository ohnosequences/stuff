
```scala
package ohnosequences.stuff.test

import ohnosequences.stuff._
import ohnosequences.stuff.functions._
import ohnosequences.stuff.products._

import scala.{ Int }
import scala.Predef.String
import org.scalatest.FunSuite

class ScalaCategoryTests extends FunSuite {

  val l     : String -> Int = λ { x: String => x.length }
  val toStr : Int -> String = λ { x: Int => x.toString  }
  // val idInt : Int -> Int = Scala.identity

  val uh: (Int -> String) -> (String -> Int) =
    Category.hom(Scala).at[Int × String, String × Int](l and l) // no good inference here
    // Category.hom(Scala)(l and l)

  test("Hom functor") {

    assert {  ( (uh at toStr) at "hola" ) === 1 }
  }

  test("composition and identity") {

    assert { Scala.composition( Scala.identity[String] and Scala.identity[String] )("hola") === "hola"  }

    assert { Scala.composition( Scala.identity[Int] and toStr )(234243) === toStr(234243)  }
  }
}

```




[test/scala/tuples/stdComparison.scala]: tuples/stdComparison.scala.md
[test/scala/tuples/syntax.scala]: tuples/syntax.scala.md
[test/scala/functors/functorExamples.scala]: functors/functorExamples.scala.md
[test/scala/sums.scala]: sums.scala.md
[test/scala/ScalaCategory.scala]: ScalaCategory.scala.md
[test/scala/functions/syntax.scala]: functions/syntax.scala.md
[test/scala/categories.scala]: categories.scala.md
[main/scala/stuff/products.scala]: ../../main/scala/stuff/products.scala.md
[main/scala/stuff/Scala.scala]: ../../main/scala/stuff/Scala.scala.md
[main/scala/stuff/package.scala]: ../../main/scala/stuff/package.scala.md
[main/scala/stuff/sums.scala]: ../../main/scala/stuff/sums.scala.md
[main/scala/stuff/boolean.scala]: ../../main/scala/stuff/boolean.scala.md
[main/scala/stuff/functors.scala]: ../../main/scala/stuff/functors.scala.md
[main/scala/stuff/naturalTransformations.scala]: ../../main/scala/stuff/naturalTransformations.scala.md
[main/scala/stuff/categories.scala]: ../../main/scala/stuff/categories.scala.md
[main/scala/stuff/functions.scala]: ../../main/scala/stuff/functions.scala.md