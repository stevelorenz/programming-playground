#
# @lc app=leetcode id=435 lang=python3
#
# [435] Non-overlapping Intervals
#

# @lc code=start
class Solution:
    def eraseOverlapIntervals(self, intervals: List[List[int]]) -> int:
        size = len(intervals)
        if size <= 1:
            return 0
        # Sort the end intervals
        intervals.sort(key=lambda x: x[1])

        prev_end = intervals[0][1]
        ret = 0
        for i in range(1, size):
            start, end = intervals[i]
            if start >= prev_end:
                # No overlapping, update the current right boundary
                prev_end = end
            else:
                # There's overlapping
                ret += 1
        return ret


# @lc code=end
