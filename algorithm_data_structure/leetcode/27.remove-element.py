#
# @lc app=leetcode id=27 lang=python3
#
# [27] Remove Element
#

# @lc code=start
class Solution:
    def removeElement(self, nums: List[int], val: int) -> int:
        ls = len(nums)
        if ls == 0:
            return ls
        count = 0
        index = 0
        # Replace the current element with the ls - 1 - count element
        while index < ls - count:
            if nums[index] == val:
                nums[index] = nums[ls - 1 - count]
                count += 1
            else:
                index += 1
        # number of rest elements
        return ls - count
# @lc code=end

