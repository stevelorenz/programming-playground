#
# @lc app=leetcode id=5 lang=python3
#
# [5] Longest Palindromic Substring
#

# @lc code=start
class Solution:
    # Yet another dynamic programming task
    def longestPalindrome(self, s: str) -> str:
        # Two situations: aba, aa

        # Use dynamic programming: bab -> c bab c is also OK

        if len(s) == 1:
            return s

        return []


# @lc code=end
