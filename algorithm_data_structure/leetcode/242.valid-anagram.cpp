/*
 * @lc app=leetcode id=242 lang=cpp
 *
 * [242] Valid Anagram
 */

// @lc code=start

#include <algorithm>
#include <string>
#include <vector>

using namespace std;

class Solution {
   public:
	bool isAnagram(string s, string t) {
		if (s.length() != t.length()) {
			return false;
		}

		vector<int> counts(26, 0);

		for (int i = 0; i < s.length(); i++) {
			counts[s[i] - 'a'] += 1;
			counts[t[i] - 'a'] -= 1;
		}

		for (int i = 0; i < 26; i++) {
			if (counts[i] > 0) {
				return false;
			}
		}
		return true;
	}
};
// @lc code=end
