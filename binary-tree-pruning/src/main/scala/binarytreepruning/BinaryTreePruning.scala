package binarytreepruning

object BinaryTreePruning {
  def TN = TreeNode.apply _
  case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])

  def run(root: TreeNode): TreeNode = root
}
