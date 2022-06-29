/*
 * @lc app=leetcode id=198 lang=cpp
 *
 * [198] House Robber
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

class Solution
{
public:
    int rob(vector<int> &nums)
    {
        if (nums.empty())
        {
            return 0;
        }

        // The state transformation depends on the two actions:
        // 1. The previous hourse has been robbed: dp[i-1]
        // 2. The previous house has NOT been robbed: dp[i-2] + nums[i]

        // Initialization

        vector<int> dp(nums.size());
        if (nums.size() >= 1)
        {
            dp[0] = nums[0];
        }

        if (nums.size() >= 2)
        {
            dp[1] = max(nums[0], nums[1]);
        }

        for (int i = 2; i < nums.size(); i++)
        {
            dp[i] = max(dp[i - 1], dp[i - 2] + nums[i]);
        }

        return dp[nums.size() - 1];
    }
};
// @lc code=end
