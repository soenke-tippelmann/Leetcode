package topk

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import topk.TopK
import scala.reflect.ClassTag

class TwoSum2Spec extends AnyWordSpec with Matchers {
  def A[T: ClassTag](values: T*) = Array.apply(values:_*)

  "Top K" should {
    "compute correctly" in {
      val values =
        Table(
          ("numbers", "k", "result"),
          // test cases from the task definition
          (A(1,1,1,2,2,3), 2, A(1,2)),
          (A(1), 1, A(1))
        )

      def toStr[T](value: Array[T]): String = "[" + value.mkString(",") + "]"

      forAll (values) {
        (numbers: Array[Int], k: Int, result: Array[Int]) => {
          println(s"\nRunning ${toStr(numbers)} with k=$k expecting ${toStr(result)}")
          TopK.topK(numbers, k) should contain theSameElementsAs result
        }
      }
    }
  }
}

