#
# @lc app=leetcode id=179 lang=python3
#
# [179] Largest Number
#

# @lc code=start
from functools import cmp_to_key


class Solution:
    def largestNumber(self, nums: List[int]) -> str:
        # This problem caused some headache to me.
        # We want to find the largest digit instead of the largest number
        # The use the greedy approach
        # Compare two elements by just brute-force comparison

        for i, n in enumerate(nums):
            nums[i] = str(n)

        def compare(n1, n2):
            # Return -1 if n1 is "smaller" than n2, so n1 comes firstly
            # Return +1 if n1 is "larger" than n2, so n2 comes firstly
            if n1 + n2 > n2 + n1:
                # n1 has the more significant digit
                return -1  # Do not swap them !
            else:
                return 1  # Swap them !

        # cmp_to_key is the method to customize the comparision function
        nums = sorted(nums, key=cmp_to_key(compare))

        if nums[0] == "0":
            ret = "0"
        else:
            ret = "".join(nums)

        return ret


# @lc code=end
