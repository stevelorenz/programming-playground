#
# @lc app=leetcode id=148 lang=python3
#
# [148] Sort List
#

# @lc code=start
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next


class Solution:
    def sortList(self, head: Optional[ListNode]) -> Optional[ListNode]:
        if not head:
            return None

        # Only one element is in the list
        if not head.next:
            return head

        nums = []

        while head.next:
            nums.append(head.val)
            head = head.next
        nums.append(head.val)
        nums.sort()

        new_head = ListNode(nums[0])
        tmp_head = new_head
        for n in nums[1:]:
            new_node = ListNode(n)
            tmp_head.next = new_node
            tmp_head = new_node
        
        return new_head


# @lc code=end
