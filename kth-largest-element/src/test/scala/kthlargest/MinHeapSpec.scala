package kthlargest

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._

class MinHeapSpec extends AnyWordSpec with Matchers {
  def L(values: Int*) = List.apply(values:_*)

  "MinHeap" should {
    "add values correctly" in {
      val values =
        Table(
          ("inserts", "result"),
          (L(), L()),
          (L(5), L(5)),
          (L(5,6), L(5,6)),
          (L(5,4), L(4,5)),
          (L(5,6,7), L(5,6,7)),
          (L(5,6,4), L(4,6,5)),
          (L(5,6,7,8), L(5,6,7,8)),
          (L(5,6,7,8,9), L(5,6,7,8,9)),
          (L(5,6,7,4), L(4,5,7,6)),
          (L(5,3,6,7,3,4,4), L(3,3,4,7,5,6,4))
        )

      forAll (values) {
        (inserts: List[Int], result: List[Int]) => {
          println(s"\nRunning $inserts expecting $result")
          MinHeap(inserts:_*).toList shouldEqual result
        }
      }
    }
    "delete values correctly" in {
      val values =
        Table(
          ("inserts", "result"),
          (L(), L()),
          (L(5), L()),
          (L(5,6), L(6)),
          (L(5,6,7), L(6,7)),
          (L(5,6,4), L(5,6)),
          (L(5,6,7,8), L(6,8,7)),
          (L(5,6,7,8,9), L(6,8,7,9)),
          (L(5,6,7,4), L(5,6,7)),
          (L(5,3,6,7,3,4,4), L(3,4,4,7,5,6)),
          (L(), L())
        )

      forAll (values) {
        (inserts: List[Int], result: List[Int]) => {
          println(s"\nRunning $inserts and 1 deletion, expecting $result")
          val heap = MinHeap(inserts:_*)
          heap.delete()
          heap.toList shouldEqual result
        }
      }
    }
  }
}

