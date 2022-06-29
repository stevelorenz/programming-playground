/*
 * @lc app=leetcode id=448 lang=cpp
 *
 * [448] Find All Numbers Disappeared in an Array
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

class Solution
{
public:
    vector<int> findDisappearedNumbers(vector<int> &nums)
    {
        vector<int> ans;

        // Mark the existing number position as negative
        int pos = 0;
        for (const int &num : nums)
        {
            pos = abs(num) - 1; // abs is needed because the following operation could flip the number
            if (nums[pos] > 0)
            {
                nums[pos] = -nums[pos];
            }
        }

        // All positive numbers are actually missing ones
        for (int i = 0; i < nums.size(); i++)
        {
            if (nums[i] > 0)
            {
                ans.push_back(i + 1);
            }
        }

        return ans;
    }
};
// @lc code=end
