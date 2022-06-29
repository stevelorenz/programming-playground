/*
 * @lc app=leetcode id=104 lang=cpp
 *
 * [104] Maximum Depth of Binary Tree
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
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
 * };
 */
class Solution
{
public:
    int maxDepth(TreeNode *root)
    {
        if (root == nullptr)
        {
            return 0;
        }

        if (not root->left and not root->right)
        {
            return 1;
        }

        int left_depth = 1;
        int right_depth = 1;

        if (root->left)
        {
            left_depth = left_depth + maxDepth(root->left);
        }

        if (root->right)
        {
            right_depth = right_depth + maxDepth(root->right);
        }

        if (left_depth > right_depth)
        {
            return left_depth;
        }
        else
        {
            return right_depth;
        }
    }
};
// @lc code=end
