#
# @lc app=leetcode id=100 lang=python3
#
# [100] Same Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def isSameTree(self, p: Optional[TreeNode], q: Optional[TreeNode]) -> bool:
        # Use DFS, for most tree problems, using DFS with recursion is easier than BFS
        def dfs(p, q):
            # When two binary trees are empty
            if p == None and q == None:
                return True
            # When only one of two binary trees is empty
            elif p == None or q == None:
                return False
            # If the binary tree is not empty, compare the value
            elif p.val == q.val:
                # When the value of the root node is the same, continue to judge the value of the left and right subtrees
                return dfs(p.left, q.left) and dfs(p.right, q.right)
            # If the value is different, false is returned
            return False

        return dfs(p, q)


# @lc code=end
