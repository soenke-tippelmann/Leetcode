package topk

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import topk.TopK
import scala.reflect.ClassTag
import scala.util.Random

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
          TopK.naive(numbers, k) should contain theSameElementsAs result
          TopK.optimized(numbers, k) should contain theSameElementsAs result
        }
      }

      val randomValues =
        Table(
          ("numbers", "k"),
          (Array.tabulate(10)(_ => Random.nextInt() % 2 - 1), 5),
          (Array.tabulate(100)(_ => Random.nextInt() % 2 - 1), 5),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 5),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 5),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 10),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 20),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 50),
          (Array.tabulate(1000)(_ => Random.nextInt() % 20 - 10), 1000)
        )

      forAll (randomValues) {
        (numbers: Array[Int], k: Int) => {
          println(s"\nRunning random sequence of size ${numbers.size} with k=$k")
          TopK.naive(numbers, k) should contain theSameElementsAs TopK.optimized(numbers, k)
        }
      }
    }
  }
}

