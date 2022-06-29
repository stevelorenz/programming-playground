/*
 * @lc app=leetcode id=101 lang=cpp
 *
 * [101] Symmetric Tree
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
    bool helper(TreeNode *left, TreeNode *right)
    {
        // Both nodes are empty! Then it's balance.
        if ((not left) and (not right))
        {
            return true;
        }

        if ((not left) or (not right))
        {
            return false;
        }

        if (left->val != right->val)
        {
            return false;
        }
        // For the symetric: left->left should be compared with right->right.
        // left->right should be compared with right->left.
        return helper(left->left, right->right) and helper(left->right, right->left);
    }

    bool isSymmetric(TreeNode *root)
    {
        if (not root)
        {
            return true;
        }
        else
        {
            return helper(root->left, root->right);
        }
    }
};
// @lc code=end
