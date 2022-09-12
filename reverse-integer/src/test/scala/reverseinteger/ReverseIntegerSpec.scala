package ReverseInteger

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import reverseinteger.ReverseInteger

class ReverseIntegerSpec extends AnyWordSpec with Matchers {

  "Reverse integer" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          // test cases from the task definition
          (123, 321),
          (-123, -321),
          (120, 21)
        )

      forAll (values) {
        (input: Int, result: Int) => {
          println(s"\nRunning $input, expecting $result")
          ReverseInteger.run(input) shouldEqual result
        }
      }
    }
  }
}

