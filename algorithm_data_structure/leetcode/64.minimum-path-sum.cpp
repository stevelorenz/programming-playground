/*
 * @lc app=leetcode id=64 lang=cpp
 *
 * [64] Minimum Path Sum
 */

// @lc code=start

#include <algorithm>
#include <vector>

using namespace std;

class Solution {
   public:
	int minPathSum(vector<vector<int>> &grid) {
		// There is only two directions: right or down
		int m = grid.size();
		int n = grid[0].size();
		// DP stores the shortest path from the position {i, j}
		vector<vector<int>> dp(m, vector<int>(n, 0));

		for (int i = 0; i < m; i++) {
			for (int j = 0; j < n; j++) {
				// The first position
				if (i == 0 and j == 0) {
					dp[i][j] = grid[i][j];
				}
				// The first row! Can not simply use i-1
				else if (i == 0) {
					dp[i][j] = dp[i][j - 1] + grid[i][j];
				}
				// The first column! Can not simply use j-1
				else if (j == 0) {
					dp[i][j] = dp[i - 1][j] + grid[i][j];
				} else {
					// Only two directions:
					// 1. From left to right: dp[i-1][j]
					// 2. From up to down: dp[i][j-1]
					dp[i][j] = min(dp[i - 1][j], dp[i][j - 1]) + grid[i][j];
				}
			}
		}
		return dp[m - 1][n - 1];
	}
};
// @lc code=end
