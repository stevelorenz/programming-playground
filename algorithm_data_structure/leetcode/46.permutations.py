#
# @lc app=leetcode id=46 lang=python3
#
# [46] Permutations
#

# @lc code=start
class Solution:
    def permute(self, nums: List[int]) -> List[List[int]]:
        # The backtracking is used to enumerate all situations
        res = []
        if len(nums) == 0:
            return res
        self.get_permute(res, nums, 0)
        return res

    def get_permute(self, res, nums, index):
        if index == len(nums):
            res.append(list(nums))
            return

        for i in range(index, len(nums)):
            # Take action!
            nums[i], nums[index] = nums[index], nums[i]
            # Backtracking
            self.get_permute(res, nums, index + 1)
            # Recover action
            nums[i], nums[index] = nums[index], nums[i]


# @lc code=end
