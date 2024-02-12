package leetcode.archerycompetition

import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec


class ArcheryCompetitionSpec extends AnyWordSpec with Matchers {

  def A(v: Int, values: Int*): Array[Int] = Array(v, values: _*)

  "Archery Competition" when {

    val solvers =
      Table(
        ("name", "solver"),
        ("exhaustive", ArcheryCompetition.maximumBobPoints _),
        ("dynamic programming", ArcheryCompetitionDynamicProgramming.maximumBobPoints _),
        ("iterative", ArcheryCompetitionIterative.maximumBobPoints _)
      )

    val taskDescriptionCases =
      Table(
        ("numArrows", "aliceArrows", "result"),
        (9, A(1, 1, 0, 1, 0, 0, 2, 1, 0, 1, 2, 0), A(0, 0, 0, 0, 1, 1, 0, 0, 1, 2, 3, 1)),
        (3, A(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 2), A(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0))
      )

    forAll(solvers) {
      (name: String, solver: (Int, Array[Int]) => Array[Int]) => {
        s"using solver $name" should {
          "compute examples from task description correctly" in {
            forAll(taskDescriptionCases) {
              (numArrows: Int, aliceArrows: Array[Int], result: Array[Int]) =>
                println(s"checking numArrows: $numArrows, aliceArrows: ${aliceArrows.mkString("[", ",", "]")}")
                solver(numArrows, aliceArrows) shouldEqual result
            }
          }
        }
      }
    }
  }
}

