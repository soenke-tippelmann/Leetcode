package binarytreepruning

import binarytreepruning.BinaryTreePruning
import binarytreepruning.BinaryTreePruning.TreeNode
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.prop.TableFor2


class BinaryTreePruningSpec extends AnyWordSpec with Matchers {

  def T(v: Int, l: Option[TreeNode], r: Option[TreeNode]): Option[TreeNode] =
    Some(TreeNode(v, l, r))
  def R(v: Int, l: Option[TreeNode], r: Option[TreeNode]): TreeNode = T(v,l,r).get
  val N: Option[TreeNode] = None
  def L(v: Int): Option[TreeNode] = T(v, N, N)

  "Binary Tree Pruning" should {
    "compute correctly" in {
      val values =
        Table(
          ("input", "result"),
          (R(1,N,N), R(1,N,N)),
          // test cases from the task definition
          (R(1,N,T(0,L(0),L(1))),
            R(1,N,T(0,N,L(1)))),
          (R(1,T(0,L(0),L(0)), T(1,L(0),L(1))),
            R(1,N,T(1,N,L(1)))),
          (R(1,T(1,T(1,L(0),N),L(1)), T(0,L(0),L(1))),
            R(1,T(1,L(1),L(1)), T(0,N,L(1)))),
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

