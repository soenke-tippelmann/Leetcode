package minimuminterval

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import minimuminterval.MinimumInterval
import scala.reflect.ClassTag
import minimuminterval.MinimumInterval.Interval

class MinimumIntervalSpec extends AnyWordSpec with Matchers {
  def A[T: ClassTag](values: T*) = Array.apply(values:_*)
  def I(left: Int, right: Int) = Interval(left, right)

  "Minimum interval" should {
    "compute correctly" in {
      val values =
        Table(
          ("intervals", "queries", "result"),
          (A(I(1,3)), A(4), A(-1)),
          (A(I(1,3)), A(2), A(3)),
          (A(I(1,2),I(5,7)), A(2,4,6,8), A(2,-1,3,-1)),
          (A(I(2,6),I(4,5)), A(1,2,4,5,6,7), A(-1,5,2,2,5,-1)),
          (A(I(2,6),I(5,7)), A(4,5,6,7,8), A(5,3,3,3,-1)),
          (A(I(4,6),I(5,9)), A(4,5,6,7), A(3,3,3,5)),
          // test cases from the task definition
          (A(I(1,4),I(2,4),I(3,6),I(4,4)), A(2,3,4,5), A(3,3,1,4)),
          (A(I(2,3),I(2,5),I(1,8),I(20,25)), A(2,19,5,22), A(2,-1,4,6))
        )

      println("Testing naive approach")
      forAll (values) {
        (intervals: Array[Interval], queries: Array[Int], result:Array[Int]) => {
          println(s"\nRunning [${intervals.mkString(", ")}], [${queries.mkString(",")}] expecting [${result.mkString(",")}]")
          MinimumInterval.naive(intervals, queries) shouldEqual result
        }
      }

      println("Testing optimized approach")
      forAll (values) {
        (intervals: Array[Interval], queries: Array[Int], result:Array[Int]) => {
          println(s"\nRunning [${intervals.mkString(", ")}], [${queries.mkString(",")}] expecting [${result.mkString(",")}]")
          MinimumInterval.optimized(intervals, queries) shouldEqual result
        }
      }
    }
  }
}

