#
# @lc app=leetcode id=373 lang=python3
#
# [373] Find K Pairs with Smallest Sums
#

# @lc code=start

import heapq


class Solution:
    def kSmallestPairs(
        self, nums1: List[int], nums2: List[int], k: int
    ) -> List[List[int]]:
        ans = []
        size1, size2 = len(nums1), len(nums2)
        if size1 * size2 == 0:
            return ans
        heap = []
        for x in range(size1):
            heapq.heappush(heap, (nums1[x] + nums2[0], x, 0))
        while len(ans) < min(k, size1 * size2):
            sum, i, j = heapq.heappop(heap)
            ans.append((nums1[i], nums2[j]))
            if j + 1 < size2:
                heapq.heappush(heap, (nums1[i] + nums2[j + 1], i, j + 1))
        return ans


# @lc code=end
