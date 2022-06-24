/*
 * @lc app=leetcode id=34 lang=cpp
 *
 * [34] Find First and Last Position of Element in Sorted Array
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

int binarySearch(vector<int> &nums, int left, int right, int target)
{

    while (left <= right)
    {
        int mid = left + (right - left) / 2;
        if (nums[mid] == target)
        {
            return mid;
        }
        if (target > nums[mid])
        {
            left = mid + 1;
        }
        if (target < nums[mid])
        {
            right = mid - 1;
        }
    }
    return -1; // Not found !
}

class Solution
{
public:
    vector<int> searchRange(vector<int> &nums, int target)
    {
        vector<int> ret;
        int left = -1, right = -1;
        int pos = binarySearch(nums, 0, nums.size() - 1, target);

        if (pos >= 0)
        {
            left = right = pos;

            // Search the range on the left of pos
            int l = left;
            do
            {
                left = l;
                l = binarySearch(nums, 0, left - 1, target);
            } while (l >= 0);

            // Search the range on the right of the pos.
            int r = right;
            do
            {
                right = r;
                r = binarySearch(nums, right + 1, nums.size() - 1, target);
            } while (r >= 0);
        }

        ret.push_back(left);
        ret.push_back(right);

        return ret;
    }
};
// @lc code=end
