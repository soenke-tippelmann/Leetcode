from tree import TreeNode

class Solution:
    """
    Solving Leet Code 2265. Count Nodes Equal to Average of Subtree
    https://leetcode.com/problems/count-nodes-equal-to-average-of-subtree/description/
    """
    def averageOfSubtree(self, root: TreeNode) -> int:
        (root_matches, root_count, root_sum) = self.__averageOfSubtree(root)
        return root_matches 

    # returns (count of matches, count of elements, sum of elements)
    def __averageOfSubtree(self, root: TreeNode) -> (int, int, int):
        if not root:
            return (0, 0, 0)

        (l_matches, l_count, l_sum) = self.__averageOfSubtree(root.left)
        (r_matches, r_count, r_sum) = self.__averageOfSubtree(root.right)

        node_sum = l_sum + r_sum + root.val
        node_count = l_count + r_count + 1

        avg = node_sum // node_count

        is_match = avg == root.val
        node_matches = l_matches + r_matches + (1 if is_match else 0)

        return (node_matches, node_count, node_sum)

