package binarytreepruning

object BinaryTreePruning {
  case class TreeNode(value: Int, left: Option[TreeNode], right: Option[TreeNode])

  // todo: unclear: what happens if the root node does not contain a 1?
  def run(root: TreeNode): TreeNode = iterate(Some(root)).get

  private def iterate(node: Option[TreeNode]): Option[TreeNode] =
    node match {
      case None => None
      case Some(TreeNode(1, l, r)) =>
        Some(TreeNode(1, iterate(l), iterate(r)))
      case Some(TreeNode(n, l, r)) =>
        val newL = iterate(l)
        val newR = iterate(r)
        if(newL.isDefined || newR.isDefined)
          Some(TreeNode(n,newL,newR))
        else
          None
    }
}

