package binarytreepruning

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import binarytreepruning.BinaryTreePruning
import org.scalatest.prop.TableDrivenPropertyChecks._
import binarytreepruning.BinaryTreePruning.TreeNode


class BinaryTreePruningSpec extends AnyWordSpec with Matchers {

  def TN = TreeNode.apply _

  "Binary Tree Pruning" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          (TN(1,None,None), TN(1,None,None)),
        )

      forAll (values) {
        (input: TreeNode, result: TreeNode) => {
          println(s"\nRunning $input, expecting $result")
          BinaryTreePruning.run(input) shouldEqual result
        }
      }
    }
  }
}
