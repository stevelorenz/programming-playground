#
# @lc app=leetcode id=56 lang=python3
#
# [56] Merge Intervals
#

# @lc code=start
class Solution:
    def merge(self, intervals: List[List[int]]) -> List[List[int]]:
        # Sort by the start value, or the left value of the interval
        intervals.sort(key=lambda i: i[0])

        # Start from the first interval
        res = [intervals[0]]

        for start, end in intervals[1:]:
            last_end = res[-1][1] # the latest largest right interval
            if start <= last_end:
                # They are overlapped, merge them without adding an new interval
                # Let the end cover these two intervals
                res[-1][1] = max(last_end, end)
            else:
                res.append([start, end])

        return res


# @lc code=end
