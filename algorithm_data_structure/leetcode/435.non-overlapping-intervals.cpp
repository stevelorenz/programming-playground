/*
 * @lc app=leetcode id=435 lang=cpp
 *
 * [435] Non-overlapping Intervals
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

class Solution
{
public:
    int eraseOverlapIntervals(vector<vector<int>> &intervals)
    {
        // Check base case
        if (intervals.empty())
        {
            return 0;
        }
        // Sort the intevals with its end value. Use a lambda.
        sort(intervals.begin(), intervals.end(),
             [](vector<int> &a, vector<int> &b)
             {
                 return a[1] < b[1];
             });

        int removed = 0;
        int prev = intervals[0][1];

        // Start the interation from the second element
        for (int i = 1; i < intervals.size(); ++i)
        {
            if (intervals[i][0] < prev)
            {
                // Find a duplicated interval. The start value of the interval
                // is smaller than the end value of the previous interval
                ++removed;
            }
            else
            {
                // A unduplicated interval
                prev = intervals[i][1];
            }
        }

        return removed;
    }
};
// @lc code=end
