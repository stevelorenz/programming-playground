#
# @lc app=leetcode id=111 lang=python3
#
# [111] Minimum Depth of Binary Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def minDepth(self, root: Optional[TreeNode]) -> int:
        # Use BFS, layer by layer !
        if not root:
            return 0

        depth = 1
        queue = []
        queue.append(root)

        while queue:
            layer_size = len(queue)
            for _ in range(layer_size):
                node = queue.pop(0)
                # Find the node with minimum depth
                if not node.left and not node.right:
                    return depth
                else:
                    if node.left:
                        queue.append(node.left)
                    if node.right:
                        queue.append(node.right)
            depth += 1

        return depth


# @lc code=end
