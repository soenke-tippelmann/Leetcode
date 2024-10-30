import unittest
from tree import TreeNode, buildTree
from main import Solution


class MyTest(unittest.TestCase):

    def test(self):
        trees = [
            ([1, [2, [3], [4, [], [5]]]], 3),
            ([4, [8, [0], [1]], [5, [], [6]]], 5),
            ([1], 1)
        ]
        for (tree, count) in trees:
            treeNode = buildTree(tree)
            print(treeNode.show())
            result = Solution().averageOfSubtree(treeNode)
            self.assertEqual(result, count)

