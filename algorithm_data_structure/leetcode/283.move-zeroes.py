#
# @lc app=leetcode id=283 lang=python3
#
# [283] Move Zeroes
#

# @lc code=start
class Solution:
    def moveZeroes(self, nums: List[int]) -> None:
        """
        Do not return anything, modify nums in-place instead.
        """
        n_pos = 0
        # Like bubblesort
        for i in range(len(nums)):
            if nums[i] != 0:
                # Swap nums[n_pos] and nums[i]
                temp = nums[n_pos]
                nums[n_pos] = nums[i]
                nums[i] = temp
                n_pos += 1


# @lc code=end
