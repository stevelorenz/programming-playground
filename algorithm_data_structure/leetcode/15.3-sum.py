#
# @lc app=leetcode id=15 lang=python3
#
# [15] 3Sum
#

# @lc code=start
class Solution:
    def threeSum(self, nums: List[int]) -> List[List[int]]:
        if len(nums) < 3:
            return []

        nums.sort()
        res = []

        # Left and right occupy two positions
        for i in range(len(nums) - 2):
            # Skip duplicated number
            if i > 0 and nums[i] == nums[i - 1]:
                continue

            # Two pointers approach
            left = i + 1
            right = len(nums) - 1  # the last number

            # Left and right can not be the same number
            while left < right:
                curr = nums[i] + nums[left] + nums[right]
                if curr == 0:
                    # Find a solution
                    res.append([nums[i], nums[left], nums[right]])
                    # skip duplicated result in the res
                    while left < right and nums[left] == nums[left + 1]:
                        left += 1
                    while left < right and nums[right] == nums[right - 1]:
                        right -= 1
                    # Move foreward
                    left += 1
                    right -= 1

                elif curr < 0:
                    # Find a bigger number
                    left += 1
                else:
                    # Try to find a small number
                    right -= 1

        # Duplicated elements can be also filtered here
        return res


# @lc code=end
