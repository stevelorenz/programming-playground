#
# @lc app=leetcode id=144 lang=python3
#
# [144] Binary Tree Preorder Traversal
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def preorderTraversal(self, root: Optional[TreeNode]) -> List[int]:
        # Use a stack and two color approach! Miao A!
        WHITE, BLACK = 0, 1
        res = []
        stack = []
        stack.append((WHITE, root))

        while stack:
            color, node = stack.pop()
            if node is None:
                continue
            if color == WHITE:
                stack.append((WHITE, node.right))
                stack.append((WHITE, node.left))
                stack.append((BLACK, node))  # Last in first out!
            else:
                res.append(node.val)

        return res

        # Use recursion
    #     if not root:
    #         return []

    #     self.res = []
    #     self.helper(root)
    #     return self.res

    # def helper(self, node):
    #     self.res.append(node.val)
    #     if node.left:
    #         self.helper(node.left)
    #     if node.right:
    #         self.helper(node.right)


# @lc code=end
