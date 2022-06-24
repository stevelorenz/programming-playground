/*
 * @lc app=leetcode id=135 lang=cpp
 *
 * [135] Candy
 */

// @lc code=start

#include <algorithm>
#include <vector>

class Solution
{
public:
    int candy(vector<int> &ratings)
    {
        /* Perform two interations: left->right + right->left */
        int size = ratings.size();
        if (size < 2)
        {
            return size;
        }

        // Init the candy number as 1
        std::vector<int> num(size, 1);

        // Iterate from left -> right
        for (int i = 1; i < num.size(); i++)
        {
            // If the current element has higher value than the left one
            if (ratings[i] > ratings[i - 1])
            {
                num[i] = num[i - 1] + 1;
            }
        }

        // Iterate from right -> left
        for (int i = size - 1; i > 0; --i)
        {
            if (ratings[i] < ratings[i - 1])
            {
                num[i - 1] = std::max(num[i - 1], num[i] + 1);
            }
        }

        return std::accumulate(num.begin(), num.end(), 0);
    }
};
// @lc code=end
