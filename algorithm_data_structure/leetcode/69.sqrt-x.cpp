/*
 * @lc app=leetcode id=69 lang=cpp
 *
 * [69] Sqrt(x)
 */

// @lc code=start

#include <stdint.h>

class Solution
{
public:
    int mySqrt(int x)
    {
        if (x < 0)
        {
            return x;
        }

        // Use left close and right close
        int left = 1, right = (x / 2) + 1, mid = 0;
        uint64_t mid_q = 0;

        while (left <= right)
        {
            mid = left + (right - left) / 2; // Avoid overflow

            mid_q = (uint64_t)mid * (uint64_t)mid;

            if (mid_q == x)
            {
                return mid;
            }
            else if (mid_q < x)
            {
                left = mid + 1;
            }
            else
            {
                right = mid - 1;
            }
        }

        return right;
    }
};
// @lc code=end
