package leetcode.nqueens

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec

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
          (4, List(L(".Q..", "...Q", "Q...", "..Q."), L("..Q.", "Q...", "...Q", ".Q.."))),
        )

      forAll(values) {
        (input: Int, result: List[List[String]]) => {
          println(s"\nRunning $input, expecting $result")
          Nqueens.naive(input) should contain theSameElementsAs result
        }
      }

      forAll(Table("size", 1, 2, 3, 4, 5, 6)) {
        (size: Int) => {
          println(s"\nRunning $size")
          Nqueens.optimized(size) should contain theSameElementsAs Nqueens.naive(size)
        }
      }

      forAll(Table("size", 7, 8, 9, 10)) {
        (size: Int) => {
          println(s"\nRunning $size")
          //Nqueens.naive(size)
          println("Optimized")
          Nqueens.optimized(size)
        }
      }
    }
  }
}

