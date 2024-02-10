package leetcode.carfleet

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec


class CarFleetSpec extends AnyWordSpec with Matchers {

  def A(v: Int, values: Int*): Array[Int] = Array(v, values: _*)

  "Car Fleet" should {
    "compute correctly" in {
      val values =
        Table(
          ("target", "position", "speed", "result"),
          // test cases from the task definition
          (12, A(10, 8, 0, 5, 3), A(2, 4, 1, 1, 3), 3),
          (10, A(3), A(3), 1),
          (100, A(0, 2, 4), A(4, 2, 1), 1)
        )

      forAll(values) {
        (target: Int, position: Array[Int], speed: Array[Int], result: Int) => {
          println(s"\nRunning target: $target, position: ${position.mkString("[", ",", "]")}, " +
            s"speed: ${speed.mkString("[", ",", "]")} expecting $result fleets.")
          CarFleet.carFleet(target, position, speed) shouldEqual result
        }
      }
    }
  }
}

