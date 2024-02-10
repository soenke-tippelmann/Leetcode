package leetcode.carfleet2

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

import scala.reflect.ClassTag


class CarFleet2Spec extends AnyWordSpec with Matchers {

  def A[T: ClassTag](values: T*): Array[T] = Array(values: _*)

  "Car Fleet 2" should {
    "task definition test cases" should {
      val values =
        Table(
          ("cars", "timings"),
          // test cases from the task definition
          (A((1, 2), (2, 1), (4, 3), (7, 2)), A(1.0, -1.0, 3.0, -1.0)),
          (A((3, 4), (5, 4), (6, 3), (9, 1)), A(2.0, 1.0, 1.5, -1.0))
        )

      "naive version compute correctly" in {
        forAll(values) {
          (cars: Array[(Int, Int)], timings: Array[Double]) => {
            println(s"\nRunning cars: ${cars.mkString("[", ",", "]")} " +
              s"expecting timing: ${timings.mkString("[", ",", "]")}")
            val actualResult = CarFleet2Naive.carFleet(cars)
            println(s"Actual timings: ${actualResult.mkString("[", ",", "]")}")
            actualResult shouldEqual timings
          }
        }
      }

      "optimized version compute correctly" in {
        forAll(values) {
          (cars: Array[(Int, Int)], timings: Array[Double]) => {
            println(s"\nRunning cars: ${cars.mkString("[", ",", "]")} " +
              s"expecting timing: ${timings.mkString("[", ",", "]")}")
            val actualResult = CarFleet2.carFleet(cars)
            println(s"Actual timings: ${actualResult.mkString("[", ",", "]")}")
            actualResult shouldEqual timings
          }
        }
      }
    }

    "in other cases" should {
      val values =
        Table(
          ("cars"),
          // test cases from the task definition
          A((1, 2), (2, 1), (4, 3), (7, 2)),
          A((3, 4), (5, 4), (6, 3), (9, 1)),
          // other test cases
          A((1,8),(2,4),(3,2),(4,1)),
          A((1,8),(2,4),(3,2),(4,1),(5,8),(6,4),(7,2),(8,1)),
          A((1,8),(2,2),(3,4),(4,1)),
        )

      "naive version and optimized version agree on the result" in {
        forAll(values) {
          (cars: Array[(Int, Int)]) => {
            println(s"\nRunning cars: ${cars.mkString("[", ",", "]")}")
            val timingNaive = CarFleet2Naive.carFleet(cars)
            println(s"Timings (naive): ${timingNaive.mkString("[", ",", "]")}")
            val timingOptimized = CarFleet2.carFleet(cars)
            println(s"Timings (optimized): ${timingOptimized.mkString("[", ",", "]")}")
            timingOptimized shouldEqual timingNaive
          }
        }
      }

      "naive version and functional version agree on the result" in {
        forAll(values) {
          (cars: Array[(Int, Int)]) => {
            println(s"\nRunning cars: ${cars.mkString("[", ",", "]")}")
            val timingNaive = CarFleet2Naive.carFleet(cars)
            println(s"Timings (naive): ${timingNaive.mkString("[", ",", "]")}")
            val timingFunctional = CarFleet2Functional.carFleet(cars)
            println(s"Timings (functional): ${timingFunctional.mkString("[", ",", "]")}")
            timingFunctional shouldEqual timingNaive
          }
        }
      }
    }
  }
}

