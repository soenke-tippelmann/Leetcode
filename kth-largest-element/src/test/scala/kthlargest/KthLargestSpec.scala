package kthlargest

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import kthlargest.KthLargest
import scala.reflect.ClassTag

class KthLargestSpec extends AnyWordSpec with Matchers {
  def L[T: ClassTag](values: T*) = List.apply(values:_*)

  "KthLargest" should {
    "compute correctly" in {
      val values =
        Table(
          ("k", "initial", "queries", "results"),
          // test cases from the task definition
          (3, L(4,5,8,2), L(3,5,10,9,4), L(4,5,5,8,8))
        )

      forAll (values) {
        (k: Int, initial: List[Int], queries: List[Int], results: List[Int]) => {
          println(s"\nRunning k $k, initial $initial, queries $queries expecting $results")
          val obj = new KthLargest(k, initial)
          queries.zip(results).foreach {
            case (query, result) => obj.add(query) shouldEqual result
          }
        }
      }
    }
  }
}

