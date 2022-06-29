/*
 * @lc app=leetcode id=48 lang=cpp
 *
 * [48] Rotate Image
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

class Solution
{
public:
    void rotate(vector<vector<int>> &matrix)
    {
        // The challenge here is the in-place requirement.
        int temp = 0;
        int n = matrix.size() - 1;
        for (int i = 0; i <= n / 2; i++)
        {
            for (int j = i; j < n - i; j++)
            {
                temp = matrix[j][n - i];

                matrix[j][n - i] = matrix[i][j];
                matrix[i][j] = matrix[n - j][i];
                matrix[n - j][i] = matrix[n - i][n - j];
                matrix[n - i][n - j] = temp;
            }
        }
    }
};
// @lc code=end
