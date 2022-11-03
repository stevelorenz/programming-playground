/*
 * @lc app=leetcode id=934 lang=cpp
 *
 * [934] Shortest Bridge
 */
// @lc code=start

#include <algorithm>
#include <queue>
#include <vector>

using namespace std;

class Solution {
   public:
	// Helpfer function to search for the first island
	void dfs(queue<pair<int, int>> &points, vector<vector<int>> &grid, int m,
			 int n, int i, int j) {
		if (i < 0 or j < 0 or i == m or j == n or grid[i][j] == 2) {
			return;
		}

		// This is the edge of the island!
		if (grid[i][j] == 0) {
			points.push({i, j});
			return;
		}

		grid[i][j] = 2;

		dfs(points, grid, m, n, i - 1, j);
		dfs(points, grid, m, n, i + 1, j);
		dfs(points, grid, m, n, i, j - 1);
		dfs(points, grid, m, n, i, j + 1);
	}

	queue<pair<int, int>> findOneIsland(vector<vector<int>> &grid) {
		int m = grid.size();
		int n = grid[0].size();
		queue<pair<int, int>> points;
		bool found_first_island = false;

		for (int i = 0; i < m; i++) {
			if (found_first_island) {
				break;
			}

			for (int j = 0; j < n; j++) {
				if (grid[i][j] == 1) {
					dfs(points, grid, m, n, i, j);
					found_first_island = true;
					break;
				}
			}
		}

		return points;
	}

	vector<int> directions{-1, 0, 1, 0, -1};

	int shortestBridge(vector<vector<int>> &grid) {
		queue<pair<int, int>> points;
		points = findOneIsland(grid);

		int x, y;
		int level = 0;

		while (not points.empty()) {
			++level;
			int points_num = points.size();
			while (points_num > 0) {
				auto [row, col] = points.front();
				points.pop();
				for (int i = 0; i < 4; i++) {
					x = row + directions[i];
					y = col + directions[i + 1];
					if (x >= 0 and y >= 0 and x < grid.size() and
						y < grid[0].size()) {
						if (grid[x][y] == 2) {
							continue;
						}
						// Find the new island!
						if (grid[x][y] == 1) {
							return level;
						}
						points.push({x, y});
						grid[x][y] == 2;
					}
				}

				points_num--;
			}
		}

		return 0;
	}
};
// @lc code=end
