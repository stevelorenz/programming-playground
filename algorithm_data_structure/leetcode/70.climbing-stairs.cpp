/*
 * @lc app=leetcode id=70 lang=cpp
 *
 * [70] Climbing Stairs
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

class Solution
{
public:
    int climbStairs(int n)
    {
        if (n < 2)
            return n;

        // There is n stages, but the index in the vector of the stage i is i - 1.
        vector<int> dp(n, 1);
        dp[0] = 1;
        dp[1] = 2;

        for (int i = 2; i < n; i++)
        {
            dp[i] = dp[i - 1] + dp[i - 2];
        }

        return dp[n - 1];
    }
};
// @lc code=end
