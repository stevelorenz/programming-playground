/*
 * @lc app=leetcode id=547 lang=cpp
 *
 * [547] Number of Provinces
 */

// @lc code=start

#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

class Solution
{
public:
    void dfs(vector<vector<int>> &isConnected, int row)
    {
        for (int col = 0; col < isConnected[row].size(); col++)
        {
            if (isConnected[row][col] == 1)
            {
                isConnected[row][col] = 2;
                dfs(isConnected, col);
            }
        }
    }

    int findCircleNum(vector<vector<int>> &isConnected)
    {
        int n = 0;

        for (int i = 0; i < isConnected.size(); i++)
        {
            for (int j = 0; j < isConnected[0].size(); j++)
            {
                if (isConnected[i][j] == 1)
                {
                    n++;
                    isConnected[i][j] = 2; // Mark current node as already selected.
                    dfs(isConnected, j);
                }
            }
        }
        return n;
    }
};
// @lc code=end
