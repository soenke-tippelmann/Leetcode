package leetcode.trappingwater

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec


class TrappingWaterSpec extends AnyWordSpec with Matchers {
  "Trapping Water" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          (List.empty, 0),
          (List(0), 0),
          (List(1, 0, 1), 1),
          (List(2, 0, 2), 2),
          (List(2, 0, 0, 2), 4),
          (List(2, 0, 1), 1),
          (List(1, 0, 2), 1),
          (List(2, 0, 1, 0, 1), 2),
          (List(2, 0, 1, 0, 1, 0, 1), 3),
          (List(2, 0, 1, 0, 0, 1, 1, 0, 1), 4),
          (List(2, 1, 2), 1),
          (List(3, 1, 2, 3), 3),
          (List(2, 0, 2, 0, 1), 3),
          (List(2, 0, 2, 0, 3), 4),
          (List(1, 0, 1, 2, 0, 2), 3),
          (List(2, 1, 0), 0),
          (List(3, 2, 1, 0, 1), 1),
          // additional cases
          (List(2, 0, 1, 2), 3),
          (List(3, 1, 2, 0, 3), 6),
          (List(2, 0, 1, 0, 2), 5),
          (List(3, 2, 0, 1, 0, 2), 5),
          (List(3, 0, 2, 0, 3), 7),
          (List(0, 1, 0, 1), 1),
          (List(0, 1, 2, 0), 0),
          (List(5), 0),
          (List(3, 0), 0),
          (List(3, 0, 2, 0, 1, 0, 2), 7),
          (List(3, 0, 2, 0, 1, 0, 2, 0, 3), 16),
          (List(3, 0, 4, 1, 3), 5),
          (List(1, 2, 3, 4, 5), 0),
          (List(5, 4, 3, 2, 1), 0),
          (List(5, 4, 3, 2, 3), 1),
          (List(3, 0, 5, 4, 3, 5, 0, 2), 8),
          (List(4, 0, 3, 0, 2, 0, 1, 0, 3, 0, 4), 27),
          (List(4, 3, 1, 2, 1, 3, 1, 4), 13),
          (List(0, 1, 0, 2, 1, 0, 1, 3, 2, 1, 2, 1), 6)
        )

      forAll(values) {
        (input: List[Int], result: Int) => {
          println(s"\nRunning $input, expecting $result")
          TrappingWater.run(input) shouldEqual result
        }
      }

      forAll(values) {
        (input: List[Int], result: Int) => {
          println(s"\nRunning $input, expecting $result")
          TrappingWaterAlternative.run(input) shouldEqual result
        }
      }
    }
  }
}

