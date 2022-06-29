/*
 * @lc app=leetcode id=46 lang=cpp
 *
 * [46] Permutations
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

class Solution
{
public:
    void backtracking(vector<int> &nums, int level, vector<vector<int>> &ans)
    {
        // Keep swap the level and level + 1 elements. So the maxmial allowed level is nums.size() - 1
        if (level == nums.size() - 1)
        {
            ans.push_back(nums);
            return;
        }

        for (int i = level; i < nums.size(); i++)
        {
            swap(nums[i], nums[level]);
            backtracking(nums, level + 1, ans); // Recursively iterate over the next position
            swap(nums[i], nums[level]);         // Backtrack to the original state
        }
    }

    vector<vector<int>> permute(vector<int> &nums)
    {
        vector<vector<int>> ans;
        backtracking(nums, 0, ans);
        return ans;
    }
};
// @lc code=end
