#
# @lc app=leetcode id=104 lang=python3
#
# [104] Maximum Depth of Binary Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def maxDepth(self, root: Optional[TreeNode]) -> int:
        # if root is None:
        #     return 0

        # # Recursive approach
        # ld = self.maxDepth(root.left)
        # rd = self.maxDepth(root.right)
        # return 1 + max(ld, rd)  # 1 for the root node

        # BFS approah
        if not root:
            return 0
        queue = [root]
        depth = 0

        while queue:
            layer_size = len(queue)
            for i in range(layer_size):
                node = queue.pop(0)
                if node.left:
                    queue.append(node.left)
                if node.right:
                    queue.append(node.right)
            # Iterate over all nodes in the current layer
            depth += 1

        return depth


# @lc code=end
