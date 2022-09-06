package leetcode.palindromenumber

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

class PalindromeNumberSpec extends AnyWordSpec with Matchers {

  "Palindrome number" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          (0, true),
          (-1, false),
          (8, true),
          (20, false),
          (11, true),
          (1231, false),
          (1221, true),
          (12321, true),
          (2147483647, false),
          // test cases from the task definition
          (121, true),
          (-121, false),
          (10, false)
        )

      forAll(values) {
        (input: Int, result: Boolean) => {
          println(s"\nRunning $input, expecting $result")
          PalindromeNumber.run(input) shouldBe result
        }
      }
    }
  }
}

