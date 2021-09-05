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
        # Use DFS ---> recurision
        def dfs(root1, root2):
            # Check special cases!
            if root1 == root2 == None:
                return True

            if not root1 or not root2:
                return False

            # Start the recursion
            res = (
                root1.val == root2.val
                and dfs(root1.left, root2.right)
                and dfs(root1.right, root2.left)
            )

            return res

        if not root:
            return True
        return dfs(root.left, root.right)


# @lc code=end
