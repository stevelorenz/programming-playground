/*
 * @lc app=leetcode id=1 lang=cpp
 *
 * [1] Two Sum
 */

// @lc code=start
class Solution
{
public:
    vector<int> twoSum(vector<int> &nums, int target)
    {
        // unordered_map is available in leetcode.
        unordered_map<int, int> m; // == Python dict
        vector<int> ret;           // == Python list

        for (auto i = 0; i < nums.size(); ++i)
        { // Can not find target - nums in the map.
            if (m.find(target - nums[i]) == m.end())
            {
                // Just record the index of the nums[i]
                m[nums[i]] = i;
            }
            // Find target - nums[i] has the index of i
            else
            {
                ret.push_back(m[target - nums[i]]);
                ret.push_back(i);
            }
        }

        return ret;
    }
};
// @lc code=end
