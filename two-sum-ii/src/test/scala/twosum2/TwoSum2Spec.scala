package twosum2

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import twosum2.TwoSum2
import scala.reflect.ClassTag

class TwoSum2Spec extends AnyWordSpec with Matchers {
  def A[T: ClassTag](values: T*) = Array.apply(values:_*)

  "Rotate image" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "target", "result"),
          (A(-1000,-800,-700,-50,1,2,3,4,5,6,50,700,800,1000,2000), 5, (5,8)),
          (A(-1000,-800,-700,-50,1,2,3,4,5,6,50), 5, (5,8)),
          (A(1,2,3,4,5,6,50,700,800,1000,2000), 5, (1,4)),
          // test cases from the task definition
          (A(2,7,11,15), 9, (1,2)),
          (A(2,3,4), 6, (1,3)),
          (A(-1,0), -1, (1,2))
        )

      def toStr[T](value: Array[T]): String = "[" + value.mkString(",") + "]"

      forAll (values) {
        (input: Array[Int], target: Int, result: (Int,Int)) => {
          println(s"\nRunning ${toStr(input)} with target $target expecting $result")
          TwoSum2.twoSum(input, target) shouldEqual result
        }
      }
    }
  }
}

