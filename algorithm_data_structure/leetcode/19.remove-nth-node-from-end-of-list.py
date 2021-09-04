#
# @lc app=leetcode id=19 lang=python3
#
# [19] Remove Nth Node From End of List
#

# @lc code=start
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    def removeNthFromEnd(self, head: Optional[ListNode], n: int) -> Optional[ListNode]:
        # Use two pointers, left + right or slow/fast
        # The distance between left and right == n
        if not head or not head.next:
            return None

        # The dummy node is used to let the left pointer point at the node before the actual node that should be deleted
        dummy_head = ListNode(0)
        dummy_head.next = head

        left = dummy_head
        right = head

        for i in range(n):
            right = right.next
            if not right:
                break

        while right:
            left = left.next
            right = right.next

        # Delete the left node
        left.next = left.next.next

        return dummy_head.next


# @lc code=end
