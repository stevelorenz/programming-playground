#
# @lc app=leetcode id=94 lang=python3
#
# [94] Binary Tree Inorder Traversal
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def inorderTraversal(self, root: TreeNode) -> List[int]:
        if root == None:
            return []

        # Recursive approah
        # res = []

        # def helper(node):
        #     if node.left:
        #         helper(node.left)
        #     res.append(node.val)
        #     if node.right:
        #         helper(node.right)

        # helper(root)
        # return res

        # Non-recursive approach -> stack
        WHITE, BLACK = 0, 1
        res = []
        stack = []
        stack.append((WHITE, root))

        while stack:
            color, node = stack.pop()
            if node is None:
                # Skip the None node
                continue
            if color == WHITE:
                stack.append((WHITE, node.right))
                stack.append((BLACK, node))
                stack.append((WHITE, node.left))
            else:
                res.append(node.val)

        return res

# @lc code=end
