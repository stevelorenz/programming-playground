#
# @lc app=leetcode id=198 lang=python3
#
# [198] House Robber
#

# @lc code=start
class Solution:
    def rob(self, nums: List[int]) -> int:
        size = len(nums)
        if size == 0:
            return 0
        # DP stands for the price when rob the i-th house.
        # The dp ranges from 0 - size -> size + 1 numbers
        dp = [0] * (size + 1)
        dp[1] = nums[0]
        for i in range(2, size + 1):
            dp[i] = max(dp[i - 1], dp[i - 2] + nums[i - 2 + 1])

        return dp[size]


# @lc code=end
