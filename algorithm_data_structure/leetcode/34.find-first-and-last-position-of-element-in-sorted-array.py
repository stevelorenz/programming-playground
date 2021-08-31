#
# @lc app=leetcode id=34 lang=python3
#
# [34] Find First and Last Position of Element in Sorted Array
#

# @lc code=start
class Solution:
    def searchRange(self, nums: List[int], target: int) -> List[int]:
        length = len(nums)
        if length == 0:
            return [-1, -1]

        # Left close, right close
        left = 0
        right = length - 1

        while left <= right:
            mid = (left + right) // 2
            if nums[mid] > target:
                right = mid - 1
            elif nums[mid] < target:
                left = mid + 1
            else:
                # ! nums[mid] == target!
                # The right is included in the results !!!
                # Find the min and max in this range !!!
                for i in range(left, right + 1):
                    if nums[i] == target:
                        # Avoid count left twice
                        if nums[left] != target:
                            left = i
                        right = i
                return [left, right]

        # Not found!
        return [-1, -1]


# @lc code=end
