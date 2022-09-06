package leetcode.binarytreepruning

import leetcode.binarytreepruning.BinaryTreePruning.TreeNode
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.wordspec.AnyWordSpec


class BinaryTreePruningSpec extends AnyWordSpec with Matchers {

  val R = TreeNode.apply _
  val T = Function.untupled(R.tupled andThen Some.apply)
  val L = T(_, N, N)
  val N = None

  "Binary Tree Pruning" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          (R(1, N, N), R(1, N, N)),
          // test cases from the task definition
          (R(1, N, T(0, L(0), L(1))),
            R(1, N, T(0, N, L(1)))),
          (R(1, T(0, L(0), L(0)), T(1, L(0), L(1))),
            R(1, N, T(1, N, L(1)))),
          (R(1, T(1, T(1, L(0), N), L(1)), T(0, L(0), L(1))),
            R(1, T(1, L(1), L(1)), T(0, N, L(1)))),
        )

      forAll(values) {
        (input: TreeNode, result: TreeNode) => {
          println(s"\nRunning $input, expecting $result")
          BinaryTreePruning.run(input) shouldEqual result
        }
      }
    }
  }
}

