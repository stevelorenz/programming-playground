#
# @lc app=leetcode id=637 lang=python3
#
# [637] Average of Levels in Binary Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right

import collections


class Solution:
    def averageOfLevels(self, root: Optional[TreeNode]) -> List[float]:
        # Use BFS, the queue is used to replace the recursion
        q = collections.deque()
        ret = []
        q.append(root)
        while q:
            size = len(q)
            row = []
            # Iterate over the current layer
            for _ in range(size):
                node = q.popleft()
                if not node:
                    continue
                # Preorder visit
                row.append(node.val)  # current value
                # Recursive for the left tree and right tree
                q.append(node.left)
                q.append(node.right)
            if row:
                ret.append(sum(row) / len(row))

        return ret


# @lc code=end
