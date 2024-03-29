package leetcode.reverseinteger

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class ReverseIntegerSpec extends AnyWordSpec with Matchers {

  "Reverse integer" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          (0, 0),
          (1, 1),
          (-1, -1),
          (12, 21),
          (-12, -21),
          (Int.MaxValue, 0),
          (2143443412, 2143443412),
          (2143443491, 1943443412),
          (2143443405, 0),
          (Int.MinValue, 0),
          (Int.MinValue + 1, 0),
          // test cases from the task definition
          (123, 321),
          (-123, -321),
          (120, 21),
          // discussion
          (2147483641, 1463847412),
          (2147483643, 0),
        )

      forAll(values) {
        (input: Int, result: Int) => {
          println(s"\nRunning $input, expecting $result")
          ReverseInteger.run(input) shouldEqual result
        }
      }
    }
  }
}

