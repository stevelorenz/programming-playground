#
# @lc app=leetcode id=75 lang=python3
#
# [75] Sort Colors
#

# @lc code=start
class Solution:
    def sortColors(self, nums: List[int]) -> None:
        """
        Do not return anything, modify nums in-place instead.
        """
        # OK! I can not use the built-in sort function...
        # nums.sort()

        # Use a naive merge sort implementation instead of the built-in sort
        def mergeSort(nums):
            if len(nums) > 1:
                mid = len(nums) // 2
                leftHalf = nums[:mid]  # A copy is made here
                rightHalf = nums[mid:]

                mergeSort(leftHalf)  # They are passed by reference
                mergeSort(rightHalf)

                # Merge the sorting results
                i = 0
                j = 0
                k = 0

                while i < len(leftHalf) and j < len(rightHalf):
                    if leftHalf[i] <= rightHalf[j]:
                        nums[k] = leftHalf[i]
                        i += 1
                    else:
                        nums[k] = rightHalf[j]
                        j += 1

                    k += 1

                while i < len(leftHalf):
                    nums[k] = leftHalf[i]
                    i += 1
                    k += 1

                while j < len(rightHalf):
                    nums[k] = rightHalf[j]
                    j += 1
                    k += 1

        mergeSort(nums)


# @lc code=end
