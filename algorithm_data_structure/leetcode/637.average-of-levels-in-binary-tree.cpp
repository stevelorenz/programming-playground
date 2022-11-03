/*
 * @lc app=leetcode id=637 lang=cpp
 *
 * [637] Average of Levels in Binary Tree
 */

// @lc code=start

#include <algorithm>
#include <queue>
#include <vector>

using namespace std;

/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode() : val(0), left(nullptr), right(nullptr) {}
 *     TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left),
 * right(right) {}
 * };
 */
class Solution {
   public:
	vector<double> averageOfLevels(TreeNode *root) {
		vector<double> ans;
		if (not root) {
			return ans;
		}

		queue<TreeNode *> q;
		q.push(root);

		while (not q.empty()) {
			int count = q.size();  // Number of nodes in current layer
			double sum = 0.0;
			// Iterate over current layer
			for (int i = 0; i < count; i++) {
				TreeNode *node = q.front();
				q.pop();
				sum = sum + node->val;

				if (node->left) {
					q.push(node->left);
				}
				if (node->right) {
					q.push(node->right);
				}
			}

			ans.push_back(sum / count);
		}

		return ans;
	}
};
// @lc code=end
