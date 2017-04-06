package ohnosequences.stuff.test

import ohnosequences.stuff._, functions._, products._, sums._

case object tailrec {

  import scala.collection._, immutable._
  import scala.Predef.???

  def foldLeft[A,Z]: (∗ -> Z) × (A × Z -> Z) -> (List[A] -> Z) =
    ???

    // rec function with state List[A] × Z (here's the space leak I guess)


  // as a recursion machine
  // everything depends on state (including the initial value)

  def op[A,U,Z]: (A × Z) + U -> U + Z =
    ???

  def init[V,Z]: ∗ + V       -> V + Z =
    ???

  // "product" (not really) the key here is that list lifts to rec machines
  // so foldLeft simply takes two recursive expressions for init and comb
  // and combines them
  // if you compose first then trace this is fusion
  // if you trace first then compose you are rec first at each step
  // the first option *is* laziness; normally faster, but you get space leaks. Where?
  // when you compose first.
  // if we could have a non-nested sum type this could be incredibly fast.

  def foldLeftS[A,Z]: (∗ -> Z) × (A × Z -> Z) -> (List[A] + (List[A] × Z) -> ((List[A] × Z) + Z)) =
    λ { zops: (∗ -> Z) × (A × Z -> Z) =>

      val z: Z = left(zops)(∗); val op: (A × Z -> Z) = right(zops)

      val init = λ { as: List[A] =>

          val out: (List[A] × Z) + Z =
            if(as.isEmpty)
              inR(z)
            else
              inL(as.drop(1) & z)
          out
        }

      val next =
        λ { asz: List[A] × Z =>
          val as = left(asz); val z = right(asz)

          val out: (List[A] × Z) + Z =
            if(as.isEmpty)
              inR(z)
            else
              inL(as.tail & op(as.head & z))
          out
        }

      either(init & next)
    }
}
