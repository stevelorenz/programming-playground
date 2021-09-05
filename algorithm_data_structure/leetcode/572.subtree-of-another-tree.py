#
# @lc app=leetcode id=572 lang=python3
#
# [572] Subtree of Another Tree
#

# @lc code=start
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def isSubtree(self, root: Optional[TreeNode], subRoot: Optional[TreeNode]) -> bool:
        # Similar is the same tree
        root_res = self.preorder(root, True)
        sub_res = self.preorder(subRoot, True)

        return sub_res in root_res

    def preorder(self, root, isLeft):
        if root is None:
            if isLeft:
                return "lnull"
            else:
                return "rnull"
        return (
            "#"
            + str(root.val)
            + " "
            + self.preorder(root.left, True)
            + " "
            + self.preorder(root.right, False)
        )


# @lc code=end
