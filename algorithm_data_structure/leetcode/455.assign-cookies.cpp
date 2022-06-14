/*
 * @lc app=leetcode id=455 lang=cpp
 *
 * [455] Assign Cookies
 */

// @lc code=start

#include <algorithm>
#include <vector>

class Solution
{
public:
    int findContentChildren(vector<int> &g, vector<int> &s)
    {
        // Greedy algorithm -> Start from the child with the minimal greed factor
        std::sort(g.begin(), g.end());
        std::sort(s.begin(), s.end());

        int child = 0;
        int cookie = 0;

        // Iterate over all children and cookies
        while (child < g.size() && cookie < s.size())
        {
            // This cookie can feed the current child.
            if (g[child] <= s[cookie])
            {
                ++child;
            }
            ++cookie;
        }

        return child;
    }
};
// @lc code=end
