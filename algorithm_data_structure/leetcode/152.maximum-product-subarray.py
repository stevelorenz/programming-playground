#
# @lc app=leetcode id=152 lang=python3
#
# [152] Maximum Product Subarray
#

# @lc code=start
class Solution:
    def maxProduct(self, nums: List[int]) -> int:
        # Yet another new headache...
        if len(nums) == 0 or not nums:
            return 0

        max_product = min_product = res = nums[0]

        for i in range(1, len(nums)):
            max_pre, min_pre = max_product, min_product
            max_product = max(
                max_pre * nums[i], nums[i], min_pre * nums[i]
            )
            min_product = min(
                max_pre * nums[i], nums[i], min_pre * nums[i]
            )
            res = max(res, max_product)

        return res


# @lc code=end
