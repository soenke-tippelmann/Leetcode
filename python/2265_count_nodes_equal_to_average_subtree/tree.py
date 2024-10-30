class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

    def show(self) -> str:
        def process_subtree(node: TreeNode) -> str:
            if node:
                padded_result = [ ("|> " if idx == 0 else "|  ") + line for (idx, line) in enumerate(node.show().splitlines()) ]
                return '\n'.join(padded_result) + '\n'
            else:
                return '|> None\n'

        return str(self.val) + '\n' + process_subtree(self.left) + process_subtree(self.right)

    def showOneline(self) -> str:
        def process_subtree(node: TreeNode) -> str:
            if node:
                return '(' + node.showOneline() + ')'
            else:
                return '(None)'

        return '(' + str(self.val) + ',' + process_subtree(self.left) + ',' + process_subtree(self.right) + ')'


def buildTree(lst: list) -> TreeNode:
    if not lst:
        return None
    return TreeNode(lst[0], buildTree(lst[1]) if len(lst) >= 2 else None, buildTree(lst[2]) if len(lst) >= 3 else None)

