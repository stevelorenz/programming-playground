/*
 * @lc app=leetcode id=543 lang=cpp
 *
 * [543] Diameter of Binary Tree
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
    int diameter = 0;

    int getMaxDepth(const TreeNode *root)
    {
        if (root == nullptr)
        {
            return 0;
        }
        int left_depth = getMaxDepth(root->left);
        int right_depth = getMaxDepth(root->right);
        // Update the current diameter at each recursion.
        diameter = max(diameter, left_depth + right_depth);

        return 1 + max(left_depth, right_depth);
    }

    int diameterOfBinaryTree(TreeNode *root)
    {
        diameter = 0;
        getMaxDepth(root);
        return diameter;
    }
};
// @lc code=end
