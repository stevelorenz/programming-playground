/*
 * @lc app=leetcode id=79 lang=cpp
 *
 * [79] Word Search
 */

// @lc code=start

#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

class Solution {
   public:
	// Backtracking is a good algorithm
	void backtracking(int row, int col, vector<vector<char>> &board,
					  string &word, bool &find, vector<vector<bool>> &visited,
					  int pos) {
		// Check if the visiting already reaches out side of the board
		if (row < 0 || row >= board.size() || col < 0 ||
			col >= board[0].size()) {
			return;
		}
		if (visited[row][col] || find || board[row][col] != word[pos]) {
			return;
		}

		// Already find a solution!!!
		if (pos == word.size() - 1) {
			find = true;
			return;
		}

		// Update the visited status before recursion
		visited[row][col] = true;
		backtracking(row + 1, col, board, word, find, visited, pos + 1);
		backtracking(row - 1, col, board, word, find, visited, pos + 1);
		backtracking(row, col + 1, board, word, find, visited, pos + 1);
		backtracking(row, col - 1, board, word, find, visited, pos + 1);
		// Recover the visitied status
		visited[row][col] = false;
	}

	// This is a typical problem that can be solved with backtracking with DFS
	// The visit status needs to be reset
	bool exist(vector<vector<char>> &board, string word) {
		if (board.empty()) {
			return false;
		}
		int row_num = board.size();
		int col_num = board[0].size();
		bool find = false;
		vector<vector<bool>> visitied(row_num, vector<bool>(col_num, false));

		for (int row = 0; row < row_num; row++) {
			for (int col = 0; col < col_num; col++) {
				backtracking(row, col, board, word, find, visitied, 0);
			}
		}

		return find;
	}
};
// @lc code=end
