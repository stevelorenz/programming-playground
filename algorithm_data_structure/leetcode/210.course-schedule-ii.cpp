/*
 * @lc app=leetcode id=210 lang=cpp
 *
 * [210] Course Schedule II
 */

// @lc code=start

#include <algorithm>
#include <queue>
#include <vector>

using namespace std;

class Solution {
   public:
	vector<int> findOrder(int numCourses, vector<vector<int>> &prerequisites) {
		// Use adjaceny list to build the graph
		vector<vector<int>> graph(numCourses, vector<int>());
		vector<int> indegree(numCourses, 0);
		vector<int> ans;

		// Here the connection of graph nodes is reverse from the task
		// description [1, 0] in the prequests -> The course 1 depends on the
		// course 0 So in the graph -> point 0 is connected to 1
		for (const auto &prerequisite : prerequisites) {
			graph[prerequisite[1]].push_back(prerequisite[0]);
			indegree[prerequisite[0]] += 1;
		}
		// So all-in-all, the nodes with zero indegree are nodes that do not
		// have any dependencies

		queue<int> q;
		for (int i = 0; i < indegree.size(); i++) {
			if (indegree[i] == 0) {
				q.push(i);
			}
		}

		while (not q.empty()) {
			int u = q.front();
			ans.push_back(
				u);	 // This step means the course u is already learned
			q.pop();
			// Check all neighbour nodes -> Courses that depends on current node
			for (auto v : graph[u]) {
				indegree[v] = indegree[v] - 1;
				// This node shoud be put into the queue (it's free now!)
				if (not indegree[v]) {
					q.push(v);
				}
			}
		}

		for (int i = 0; i < indegree.size(); i++) {
			if (indegree[i] > 0) {
				return vector<int>();
			}
		}

		return ans;
	}
};
// @lc code=end
