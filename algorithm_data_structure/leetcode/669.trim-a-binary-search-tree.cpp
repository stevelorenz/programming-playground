/*
 * @lc app=leetcode id=669 lang=cpp
 *
 * [669] Trim a Binary Search Tree
 */

// @lc code=start

#include <algorithm>
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
	TreeNode *trimBST(TreeNode *root, int low, int high) {
		// Check the base case
		if (not root) {
			return root;
		}
		// Perform the actual trim action
		// Drop the right tree
		if (root->val > high) {
			return trimBST(root->left, low, high);
		}
		// Drop the left tree
		if (root->val < low) {
			return trimBST(root->right, low, high);
		}
		// Recursivly perform the action of left and right tree.
		root->left = trimBST(root->left, low, high);
		root->right = trimBST(root->right, low, high);

		return root;
	}
};
// @lc code=end
