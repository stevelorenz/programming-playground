#
# @lc app=leetcode id=3 lang=python3
#
# [3] Longest Substring Without Repeating Characters
#

# @lc code=start
class Solution:
    def lengthOfLongestSubstring(self, s: str) -> int:
        # left, right = 0, 0
        # chars = set()
        # res = 0
        # while left < len(s) and right < len(s):
        #     if s[right] in chars:
        #         # The right char is already in the set, so duplicated.
        #         # We need to re-start the set.
        #         if s[left] in chars:
        #             chars.remove(s[left])
        #         left += 1
        #     else:
        #         # The length of the substring is increased by one
        #         chars.add(s[right])
        #         right += 1
        #         res = max(res, len(chars))
        # return res

        # max_len = i - start
        if s == " ":
            return 1

        start = -1
        max_len = 0
        d = {}

        # Iterate over all characters
        for i in range(len(s)):
            # s[i] is already in the dictionary
            if s[i] in d and d[s[i]] > start:
                # Update the start index to the new position
                start = d[s[i]]
                d[s[i]] = i  # Update the old d[s[i]]
            # s[i] is not in dictionary, the maximal length can be updated !
            else:
                d[s[i]] = i
                if i - start > max_len:
                    max_len = i - start

        return max_len


# @lc code=end
