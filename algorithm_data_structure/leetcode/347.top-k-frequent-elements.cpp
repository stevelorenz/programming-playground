/*
 * @lc app=leetcode id=347 lang=cpp
 *
 * [347] Top K Frequent Elements
 */

// @lc code=start

#include <algorithm>
#include <iostream>
#include <queue>
#include <unordered_map>
#include <vector>

using namespace std;

class Solution {
   public:
	template <typename K, typename V>
	void printMap(unordered_map<K, V> const &m) {
		for (auto const &pair : m) {
			cout << "{" << pair.first << ": " << pair.second << "}" << endl;
		}
	}

	vector<int> useBucketSort(const vector<int> &nums, int k) {
		unordered_map<int, int> counts;	 // Initial value is zero
		int max_count = 0;

		for (const int &num : nums) {
			max_count = max(max_count, ++counts[num]);
		}
		// printMap(counts);

		// Buckets sorting.
		vector<vector<int>> buckets(max_count + 1);
		for (const auto &p : counts) {
			buckets[p.second].push_back(p.first);
		}

		vector<int> ret;
		// Iterate from the number max_count to 0
		for (size_t i = max_count; i >= 0 && ret.size() < k; i--) {
			for (const int &num : buckets[i]) {
				ret.push_back(num);
				if (ret.size() == k) {
					break;
				}
			}
		}

		return ret;
	}

	class element {
		int number, frequency;
		bool operator<(const element arg) const {
			return frequency < arg.frequency;
		}
	};

	vector<int> topKFrequent(vector<int> &nums, int k) {
		return useBucketSort(nums, k);
	}
};
// @lc code=end
