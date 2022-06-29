/*
 * @lc app=leetcode id=110 lang=cpp
 *
 * [110] Balanced Binary Tree
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
    int getTreeDepth(TreeNode *root)
    {
        if (root == nullptr)
        {
            return 0;
        }

        int left_depth = 1;
        int right_depth = 1;

        left_depth += getTreeDepth(root->left);
        right_depth += getTreeDepth(root->right);

        if (left_depth > right_depth)
        {
            return left_depth;
        }
        else
        {
            return right_depth;
        }
    }

    bool isBalanced(TreeNode *root)
    {
        if (root == nullptr)
        {
            return true;
        }

        int left_depth = getTreeDepth(root->left);
        int right_depth = getTreeDepth(root->right);

        if (left_depth - right_depth > 1 or right_depth - left_depth > 1)
        {
            return false;
        }
        // Recursively check the subtrees.
        return isBalanced(root->left) and isBalanced(root->right);
    }
};
// @lc code=end
