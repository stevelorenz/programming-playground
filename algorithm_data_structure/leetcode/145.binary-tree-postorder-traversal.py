#
# @lc app=leetcode id=145 lang=python3
#
# [145] Binary Tree Postorder Traversal
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def postorderTraversal(self, root: Optional[TreeNode]) -> List[int]:
        pass
        # Use stack
        if not root:
            return []

        res = []
        WHITE, BLACK = 0, 1
        stack = [(WHITE, root)]

        while stack:
            color, node = stack.pop()
            if not node:
                continue
            if color == WHITE:
                # Post-order -> left, right, root
                # Then in stack order -> root, right, left
                stack.append((BLACK, node))
                stack.append((WHITE, node.right))
                stack.append((WHITE, node.left))

            else:
                res.append(node.val)

        return res


# @lc code=end
