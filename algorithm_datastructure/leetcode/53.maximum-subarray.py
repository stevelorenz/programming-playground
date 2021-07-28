#
# @lc app=leetcode id=53 lang=python3
#
# [53] Maximum Subarray
#

# @lc code=start
class Solution:
    def maxSubArray(self, nums: List[int]) -> int:
        if len(nums) == 1:
            return nums[0]

        pre = nums[0]
        res = pre
        for i in range(1, len(nums)):
            # If nums[i] is larger than the previous one, the previous result is negative
            pre = max(nums[i], pre + nums[i])
            res = max(res, pre)

        return res


# @lc code=end
