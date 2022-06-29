/*
 * @lc app=leetcode id=437 lang=cpp
 *
 * [437] Path Sum III
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
    int pathSumStartWithRoot(TreeNode *root, int targetSum)
    {
        if (not root)
        {
            return 0;
        }
        int count = 0;
        if (root->val == targetSum)
        {
            count = 1;
        }

        count = count + pathSumStartWithRoot(root->left, targetSum - root->val);
        count = count + pathSumStartWithRoot(root->right, targetSum - root->val);

        return count;
    }

    int pathSum(TreeNode *root, int targetSum)
    {
        if (not root)
        {
            return 0;
        }

        return (pathSumStartWithRoot(root, targetSum) + pathSumStartWithRoot(root->left, targetSum) + pathSumStartWithRoot(root->right, targetSum));
    }
};
// @lc code=end
