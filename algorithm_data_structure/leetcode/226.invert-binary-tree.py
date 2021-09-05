#
# @lc app=leetcode id=226 lang=python3
#
# [226] Invert Binary Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def invertTree(self, root: Optional[TreeNode]) -> Optional[TreeNode]:
        if not root:
            return None

        # # Pre-order with BFS
        # queue = [root]

        # while queue:
        #     curr = queue.pop(0)
        #     curr.left, curr.right = curr.right, curr.left
        #     if curr.left:
        #         queue.append(curr.left)
        #     if curr.right:
        #         queue.append(curr.right)

        # return root

        # Recursive

        def dfs(root):
            # Firstly swap the left and right sub-trees.
            # Then swap the left and right node of the current node
            if root:
                dfs(root.left)
                dfs(root.right)
                root.left, root.right = root.right, root.left

        dfs(root)
        return root


# @lc code=end
