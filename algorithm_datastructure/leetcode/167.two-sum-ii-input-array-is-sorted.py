#
# @lc app=leetcode id=167 lang=python3
#
# [167] Two Sum II - Input array is sorted
#

# @lc code=start
class Solution:
    def twoSum(self, numbers: List[int], target: int) -> List[int]:
        left = 0
        right = len(numbers) - 1

        while left < right:
            sum = numbers[left] + numbers[right]
            if sum == target:
                break
            if sum < target:
                left += 1
            else:
                right -= 1
        # Return the 1-indexed indices
        return [left + 1, right + 1]


# @lc code=end
