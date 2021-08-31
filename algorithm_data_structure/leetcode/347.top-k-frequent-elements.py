#
# @lc app=leetcode id=347 lang=python3
#
# [347] Top K Frequent Elements
#

# @lc code=start
class Solution:
    def topKFrequent(self, nums: List[int], k: int) -> List[int]:
        counter = {}
        for i in nums:
            if i not in counter:
                counter[i] = 1
            else:
                counter[i] += 1

        # Sort the result
        sorted_counter = sorted(counter.items(), key=lambda x: x[1], reverse=True)

        ret = [sorted_counter[i][0] for i in range(k)]
        return ret


# @lc code=end
