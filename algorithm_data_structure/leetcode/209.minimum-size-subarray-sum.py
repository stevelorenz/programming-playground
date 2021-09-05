#
# @lc app=leetcode id=209 lang=python3
#
# [209] Minimum Size Subarray Sum
#

# @lc code=start

import math


class Solution:
    def minSubArrayLen(self, target: int, nums: List[int]) -> int:
        pass
        # Use double pointers approach
        if target > sum(nums):
            return 0

        left = 0
        right = 0
        curr_sum = 0
        res = len(nums) + 1

        while right < len(nums):
            curr_sum += nums[right]

            # Try to reduce the length of the subarray
            while curr_sum >= target:
                res = min(res, right - left + 1)
                curr_sum -= nums[left]
                left += 1

            right += 1

        if res == len(nums) + 1:
            return 0

        return res


# @lc code=end
