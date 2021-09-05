#
# @lc app=leetcode id=103 lang=python3
#
# [103] Binary Tree Zigzag Level Order Traversal
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def zigzagLevelOrder(self, root: Optional[TreeNode]) -> List[List[int]]:
        pass
        # BFS level order
        if not root:
            return []

        q = [[root]]

        for level in q:
            record = []
            for node in level:
                if node.left:
                    record.append(node.left)
                if node.right:
                    record.append(node.right)
            if record:
                q.append(record)

        # Zigzag order
        res = []
        for index, level in enumerate(q):
            temp = [x.val for x in level]
            if index % 2 == 0:
                res.append(temp)
            else:
                res.append(temp[::-1])
        return res


# @lc code=end
