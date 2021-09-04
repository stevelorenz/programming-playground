#
# @lc app=leetcode id=24 lang=python3
#
# [24] Swap Nodes in Pairs
#

# @lc code=start
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    def swapPairs(self, head: Optional[ListNode]) -> Optional[ListNode]:
        # Use a dummy node
        dummy_head = ListNode(-1)
        dummy_head.next = head
        prev, curr = dummy_head, head

        # Make sure there's at least two nodes
        while curr and curr.next:
            # Save pointers
            next_pair = curr.next.next
            second = curr.next
            # Reverse the pair
            second.next = curr
            curr.next = next_pair
            prev.next = second
            # Update the pointers
            prev = curr
            curr = next_pair

        return dummy_head.next


# @lc code=end
