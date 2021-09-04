#
# @lc app=leetcode id=445 lang=python3
#
# [445] Add Two Numbers II
#

# @lc code=start
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
from typing import List


class Solution:
    def addTwoNumbers(
        self, l1: Optional[ListNode], l2: Optional[ListNode]
    ) -> Optional[ListNode]:
        stack1 = []
        stack2 = []
        dummy_head = ListNode(-1)

        # Push all nodes in the stack
        def push_stack(p, stack):
            while p:
                stack.append(p.val)
                p = p.next

        push_stack(l1, stack1)
        push_stack(l2, stack2)

        carry = 0
        while stack1 or stack2:
            tmp1, tmp2 = 0, 0
            if stack1:
                tmp1 = stack1.pop()
            if stack2:
                tmp2 = stack2.pop()
            mod = (tmp1 + tmp2 + carry) % 10
            carry = (tmp1 + tmp2 + carry) // 10
            new_node = ListNode(mod)
            new_node.next = dummy_head.next
            dummy_head.next = new_node

        if carry > 0:
            new_node = ListNode(carry)
            new_node.next = dummy_head.next
            dummy_head.next = new_node

        return dummy_head.next


# @lc code=end
