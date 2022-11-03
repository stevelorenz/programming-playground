/*
 * @lc app=leetcode id=695 lang=cpp
 *
 * [695] Max Area of Island
 */

// @lc code=start

#include <algorithm>
#include <iostream>
#include <unordered_map>
#include <vector>

using namespace std;
class Solution {
   public:
	// {-1, 0} -> left, {0, 1} -> up
	// {+1, 0} -> right, {0, -1} -> down
	vector<int> directions{-1, 0, 1, 0, -1};

	int dfs(vector<vector<int>> &grid, int row, int col) {
		// Check base case, namely when to return the value
		if (grid[row][col] == 0) {
			return 0;
		}
		// Mark this land as already searched
		grid[row][col] = 0;

		int x = 0, y = 0, area = 1;	 // The area == 1, so means that this island
		// Search over different directions
		for (int i = 0; i < 4; i++) {
			x = row + directions[i];
			y = col + directions[i + 1];
			// Firstly check the boundary before recursion
			if (x >= 0 && x < grid.size() && y >= 0 && y < grid[0].size()) {
				// DFS recursive searching -> Like a stack.
				area = area + dfs(grid, x, y);
			}
		}

		return area;
	}

	int maxAreaOfIsland(vector<vector<int>> &grid) {
		// Check base case
		if (grid.empty() || grid[0].empty()) {
			return 0;
		}

		int max_area = 0;

		// Iterate over the map with DFS searching
		for (int i = 0; i < grid.size(); i++) {
			for (int j = 0; j < grid[0].size(); j++) {
				if (grid[i][j] == 1) {
					max_area = max(max_area, dfs(grid, i, j));
				}
			}
		}

		return max_area;
	}
};
// @lc code=end
