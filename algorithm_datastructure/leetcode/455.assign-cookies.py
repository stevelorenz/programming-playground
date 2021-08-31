#
# @lc app=leetcode id=455 lang=python3
#
# [455] Assign Cookies
#

# @lc code=start
class Solution:
    def findContentChildren(self, g: List[int], s: List[int]) -> int:
        # Greedy algorithm

        # Sort to easily get the largest g and s.
        g.sort()
        s.sort()

        num_children = 0
        i = len(g) - 1
        j = len(s) - 1

        while i >= 0 and j >= 0:
            if s[j] >= g[i]:
                num_children += 1
                i = i - 1
                j = j - 1
            else:
                # Move to the next child, but still uses the largest cookie
                i = i - 1

        return num_children


# @lc code=end
