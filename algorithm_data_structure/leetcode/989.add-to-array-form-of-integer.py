#
# @lc app=leetcode id=989 lang=python3
#
# [989] Add to Array-Form of Integer
#

# @lc code=start
class Solution:
    def addToArrayForm(self, num: List[int], k: int) -> List[int]:
        if len(num) == 1:
            return list(str(num[0] + k))

        # Convert list to number
        N = len(num)
        sum_ = 0
        for i, n in enumerate(num):
            sum_ += n * 10 ** (N - i - 1)
        sum_ += k

        # Covert integer to list
        res = list(str(sum_))
        return res


# @lc code=end
