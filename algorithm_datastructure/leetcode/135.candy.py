#
# @lc app=leetcode id=135 lang=python3
#
# [135] Candy
#

# @lc code=start


class Solution:
    def candy(self, ratings: List[int]) -> int:
        # Greedy algorithm using 2 passes
        n = len(ratings)
        if n < 2:
            return n

        ret = [1] * n

        # Scan from left to right
        for i in range(1, n, 1):
            if ratings[i] > ratings[i - 1]:
                ret[i] = ret[i - 1] + 1

        # Scan from right to left
        for i in range(n - 1, 0, -1):
            if ratings[i - 1] > ratings[i] and ret[i - 1] <= ret[i]:
                ret[i - 1] = ret[i] + 1

        return sum(ret)


# @lc code=end
