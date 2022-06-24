/*
 * @lc app=leetcode id=417 lang=cpp
 *
 * [417] Pacific Atlantic Water Flow
 */

// @lc code=start

#include <algorithm>
#include <vector>
#include <unordered_map>

using namespace std;

class Solution
{
public:
    vector<int> directions{-1, 0, 1, 0, -1};

    void dfs(const vector<vector<int>> &heights, vector<vector<bool>> &can_reach, int row, int col)
    {
        if (can_reach[row][col])
        {
            return;
        }
        can_reach[row][col] = true;

        int x = 0;
        int y = 0;

        for (int i = 0; i < 4; i++)
        {
            x = row + directions[i];
            y = col + directions[i + 1];

            // Check if the new position is valid!
            if (x >= 0 && x < heights.size() && y >= 0 && y < heights[0].size() && heights[row][col] <= heights[x][y])
            {
                // Continue to search recursively
                dfs(heights, can_reach, x, y);
            }
        }
    }

    vector<vector<int>> pacificAtlantic(vector<vector<int>> &heights)
    {
        // Check base cases
        if (heights.empty() || heights[0].empty())
        {
            return {};
        }

        vector<vector<int>> ret;
        int row_num = heights.size();
        int col_num = heights[0].size();
        vector<vector<bool>> can_reach_p(row_num, vector<bool>(col_num, false));
        vector<vector<bool>> can_reach_a(row_num, vector<bool>(col_num, false));

        // Search over rows
        for (int i = 0; i < row_num; i++)
        {
            dfs(heights, can_reach_p, i, 0);
            dfs(heights, can_reach_a, i, col_num - 1);
        }

        // Search over columns
        for (int j = 0; j < col_num; j++)
        {
            dfs(heights, can_reach_p, 0, j);
            dfs(heights, can_reach_a, row_num - 1, j);
        }

        // Get results
        for (int i = 0; i < row_num; i++)
        {
            for (int j = 0; j < col_num; j++)
            {
                if (can_reach_p[i][j] && can_reach_a[i][j])
                {
                    // Store this element in the vector
                    ret.push_back(vector<int>{i, j});
                }
            }
        }

        return ret;
    }
};
// @lc code=end
