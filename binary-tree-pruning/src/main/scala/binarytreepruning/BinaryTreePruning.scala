package binarytreepruning

object BinaryTreePruning {
  case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])

  // todo: unclear: what should happen if the root node does not contain a 1?
  // In this case the current code throws an exception
  def run(root: TreeNode): TreeNode = iterate(Some(root)).get

  private def iterate(node: Option[TreeNode]): Option[TreeNode] =
    node
      .map {
        case TreeNode(n, l, r) =>
          TreeNode(n, iterate(l), iterate(r))
      }
      .filter {
        case TreeNode(1, _, _) => true
        case TreeNode(_, None, None) => false
        case _ => true
      }
}

