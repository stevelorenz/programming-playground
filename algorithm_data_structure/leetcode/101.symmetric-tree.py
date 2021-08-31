#
# @lc app=leetcode id=101 lang=python3
#
# [101] Symmetric Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def isSymmetric(self, root: Optional[TreeNode]) -> bool:
        if not root:
            return True
        return self.mirror_visite(root.left, root.right)

    def mirror_visite(self, left, right):
        if not left and not right:
            return True
        try:
            if left.val == right.val:
                if self.mirror_visite(left.left, right.right) and self.mirror_visite(
                    left.right, right.left
                ):
                    return True
            return False
        except:
            return False


# @lc code=end
