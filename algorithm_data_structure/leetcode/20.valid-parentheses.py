#
# @lc app=leetcode id=20 lang=python3
#
# [20] Valid Parentheses
#

# @lc code=start
class Solution:
    def isValid(self, s: str) -> bool:
        # Use stack
        stack = []
        d = {"(": ")", "[": "]", "{": "}"}
        for p in s:
            if p in "{[(":
                stack.append(p)
            else:
                # Not stack means no corresponded element to match
                if not stack or d[stack.pop()] != p:
                    return False
        # The stack should be empty! Otherwise there's redudant elements.
        return not stack


# @lc code=end
