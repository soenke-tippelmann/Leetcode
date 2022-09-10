package nqueens

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import nqueens.Nqueens

class NqueensSpec extends AnyWordSpec with Matchers {
  def L(rows: String*) = List(rows: _*)

  "N queens" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          // test cases from the task definition
          (1, List(L("Q"))),
          (2, List()),
          (3, List()),
          (4, List(L(".Q..","...Q","Q...","..Q."), L("..Q.","Q...","...Q",".Q.."))),
        )

      forAll (values) {
        (input: Int, result: List[List[String]]) => {
          println(s"\nRunning $input, expecting $result")
          Nqueens.run(input) should contain theSameElementsAs result
        }
      }
    }
  }
}

