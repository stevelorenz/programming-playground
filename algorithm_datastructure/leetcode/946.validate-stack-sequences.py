#
# @lc app=leetcode id=946 lang=python3
#
# [946] Validate Stack Sequences
#

# @lc code=start
class Solution:
    def validateStackSequences(self, pushed: List[int], popped: List[int]) -> bool:
        stack = []
        i = 0
        for item in pushed:
            stack.append(item)
            while stack and popped[i] == stack[-1]:
                # A pop operation is performed.
                stack.pop()
                i += 1
        # The stack must be empty after these operations
        return stack == []


# @lc code=end
