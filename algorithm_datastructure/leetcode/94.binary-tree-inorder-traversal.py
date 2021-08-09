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

        res = []
        res += self.inorderTraversal(root.left)
        # In order, to the value of current point is in the middle
        res.append(root.val)
        res += self.inorderTraversal(root.right)
        return res


# @lc code=end
