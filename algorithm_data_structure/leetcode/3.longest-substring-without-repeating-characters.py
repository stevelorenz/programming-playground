#
# @lc app=leetcode id=3 lang=python3
#
# [3] Longest Substring Without Repeating Characters
#

# @lc code=start
class Solution:
    def lengthOfLongestSubstring(self, s: str) -> int:
        left, right = 0, 0
        chars = set()
        res = 0
        while left < len(s) and right < len(s):
            if s[right] in chars:
                # The right char is already in the set, so duplicated.
                # We need to re-start the set.
                if s[left] in chars:
                    chars.remove(s[left])
                left += 1
            else:
                # The length of the substring is increased by one
                chars.add(s[right])
                right += 1
                res = max(res, len(chars))
        return res


# @lc code=end
