package minimuminterval

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import minimuminterval.MinimumInterval
import scala.reflect.ClassTag

class MinimumIntervalSpec extends AnyWordSpec with Matchers {
  def A[T: ClassTag](values: T*) = Array.apply(values:_*)

  "Minimum interval" should {
    "compute correctly" in {
      val values =
        Table(
          ("intervals", "queries", "result"),
          (A(A(1,3)), A(4), A(-1)),
          (A(A(1,3)), A(2), A(3)),
          // test cases from the task definition
          (A(A(1,4),A(2,4),A(3,6),A(4,4)), A(2,3,4,5), A(3,3,1,4)),
          (A(A(2,3),A(2,5),A(1,8),A(20,25)), A(2,19,5,22), A(2,-1,4,6))
        )

      forAll (values) {
        (intervals: Array[Array[Int]], queries: Array[Int], result:Array[Int]) => {
          println(s"\nRunning $intervals, $queries expecting $result")
          MinimumInterval.naive(intervals, queries) shouldEqual result
        }
      }
    }
  }
}

