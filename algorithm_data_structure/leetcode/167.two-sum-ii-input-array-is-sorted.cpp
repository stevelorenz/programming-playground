/*
 * @lc app=leetcode id=167 lang=cpp
 *
 * [167] Two Sum II - Input Array Is Sorted
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

class Solution
{
public:
    vector<int> twoSum(vector<int> &numbers, int target)
    {
        // The numbers are already sorted -> can use the two pointers
        int left = 0;
        int right = numbers.size() - 1;
        int sum = 0;

        while (left < right)
        {
            sum = numbers[left] + numbers[right];
            if (sum == target)
            {
                break;
            }
            else if (sum < target)
            {
                ++left;
            }
            else
            {
                --right;
            }
        }
        // +1 Because it uses the math-index instead of 0-based index
        return vector<int>{left + 1, right + 1};
    }
};
// @lc code=end
