/*
 * @lc app=leetcode id=144 lang=cpp
 *
 * [144] Binary Tree Preorder Traversal
 */

// @lc code=start

#include <algorithm>
#include <stack>
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
	vector<int> preorderTraversal(TreeNode *root) {
		vector<int> ans;
		if (not root) {
			return ans;
		}

		stack<TreeNode *> s;
		s.push(root);

		while (not s.empty()) {
			TreeNode *node = s.top();
			s.pop();
			ans.push_back(node->val);

			// Firstly push the right side in the queue, so the left node is
			// firstly poped out from the stack
			if (node->right) {
				s.push(node->right);
			}

			if (node->left) {
				s.push(node->left);
			}
		}

		return ans;
	}
};
// @lc code=end
