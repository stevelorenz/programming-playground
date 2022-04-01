#
# @lc app=leetcode id=322 lang=python3
#
# [322] Coin Change
#

# @lc code=start


class Solution:
    def coinChange(self, coins: List[int], amount: int) -> int:
        # Bottom up DP
        if amount == 0:
            return 0
        if coins is None or len(coins) == 0:
            return -1
        coins.sort()

        # From 0 to amount
        dp = [amount + 1] * (amount + 1)

        for i in range(1, amount + 1):
            for coin in coins:
                if i < coin:
                    # Can not find a solution!
                    continue
                elif i == coin:
                    # Just use one coin
                    dp[i] = 1
                    break
                else:
                    dp[i] = min(dp[i], dp[i - coin] + 1)
        
        if dp[amount] == amount + 1:
            # No solution is found
            return -1
        else:
            return dp[amount]

# @lc code=end
