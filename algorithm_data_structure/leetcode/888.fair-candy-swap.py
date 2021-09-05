#
# @lc app=leetcode id=888 lang=python3
#
# [888] Fair Candy Swap
#

# @lc code=start
class Solution:
    def fairCandySwap(self, aliceSizes: List[int], bobSizes: List[int]) -> List[int]:
        sum_alice = sum(aliceSizes)
        sum_bob = sum(bobSizes)
        set_bob = set(bobSizes)

        target = (sum_alice + sum_bob) // 2

        for a in aliceSizes:
            expect_b = target - (sum_alice - a)
            if expect_b in set_bob:
                return [a, expect_b]

        return []


# @lc code=end
