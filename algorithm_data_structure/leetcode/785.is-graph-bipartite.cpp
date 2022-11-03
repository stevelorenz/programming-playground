/*
 * @lc app=leetcode id=785 lang=cpp
 *
 * [785] Is Graph Bipartite?
 */

// @lc code=start

#include <algorithm>
#include <queue>
#include <vector>

using namespace std;

class Solution {
   public:
	bool isBipartite(vector<vector<int>> &graph) {
		int n = graph.size();

		if (n == 0) {
			return true;
		}

		vector<int> color(n, 0);
		queue<int> q;

		// Iterate over all nodes
		for (int i = 0; i < n; i++) {
			if (not color[i]) {
				q.push(i);
				color[i] = 1;
			}

			// Use BFS to search for all adjacent nodes
			while (not q.empty()) {
				int node = q.front();
				q.pop();
				for (const int &neighbour : graph[node]) {
					// If the neighbour is not colored yet, color it with a
					// different color
					if (color[neighbour] == 0) {
						q.push(neighbour);	// Add this neighbour into the BFS
											// search
						if (color[node] == 1) {
							color[neighbour] = 2;
						} else {
							color[neighbour] = 1;
						}
					}
					// This neighbour is already colored
					else {
						if (color[node] == color[neighbour]) {
							return false;
						}
					}
				}
			}
		}

		// Didn't find any neighbours with the same color.
		return true;
	}
};
// @lc code=end
