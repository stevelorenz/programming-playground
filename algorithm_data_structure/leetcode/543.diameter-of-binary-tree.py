#
# @lc app=leetcode id=543 lang=python3
#
# [543] Diameter of Binary Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def diameterOfBinaryTree(self, root: Optional[TreeNode]) -> int:
        self.max_node_num = 1

        def depth(node):
            if not node:
                return 0
            ld = depth(node.left)
            rd = depth(node.right)
            self.max_node_num = max(self.max_node_num, ld + rd + 1)
            return max(ld, rd) + 1

        depth(root)
        return self.max_node_num - 1


# @lc code=end
