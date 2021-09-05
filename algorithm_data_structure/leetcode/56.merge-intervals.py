#
# @lc app=leetcode id=56 lang=python3
#
# [56] Merge Intervals
#

# @lc code=start
class Solution:
    def merge(self, intervals: List[List[int]]) -> List[List[int]]:
        # Sort by the start value
        intervals.sort(key=lambda i: i[0])
        res = [intervals[0]]
        for start, end in intervals[1:]:
            last_end = res[-1][1]
            if start <= last_end:
                # They are overlapped, merge them
                # Let the end cover these two intervals
                res[-1][1] = max(last_end, end)
            else:
                res.append([start, end])

        return res


# @lc code=end
