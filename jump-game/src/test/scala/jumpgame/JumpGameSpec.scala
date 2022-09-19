package jumpgame

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import jumpgame.JumpGame
import scala.reflect.ClassTag

class JumpGameSpec extends AnyWordSpec with Matchers {
  def A[T: ClassTag](values: T*) = Array.apply(values:_*)

  "Jump Game" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          // test cases from the task definition
          (A(2,3,1,1,4), true),
          (A(3,2,1,0,4), false)
        )

      forAll (values) {
        (input: Array[Int], result: Boolean) => {
          println(s"\nRunning [${input.mkString(",")}] expecting $result")
          JumpGame.run(input) shouldBe result
        }
      }
    }
  }
}

