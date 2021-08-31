#
# @lc app=leetcode id=110 lang=python3
#
# [110] Balanced Binary Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def isBalanced(self, root: Optional[TreeNode]) -> bool:
        if root is None:
            return True

        if self.get_depth(root) < 0:
            return False
        else:
            return True

    def get_depth(self, node):
        if node is None:
            return 1
        ld = self.get_depth(node.left)
        if ld < 0:
            return -1
        rd = self.get_depth(node.right)
        if rd < 0:
            return -1
        elif abs(ld - rd) > 1:
            return -1
        else:
            return max(ld, rd) + 1


# @lc code=end
