/*
 * @lc app=leetcode id=142 lang=cpp
 *
 * [142] Linked List Cycle II
 */

// @lc code=start
/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     ListNode *next;
 *     ListNode(int x) : val(x), next(NULL) {}
 * };
 */
class Solution {
   public:
	ListNode *detectCycle(ListNode *head) {
		// Linked-list -> Slow/fast pointer
		ListNode *slow = head;
		ListNode *fast = head;

		do {
			if (!fast || !fast->next) {
				return nullptr;
			}
			fast = fast->next->next;
			slow = slow->next;
		} while (fast != slow);

		// Exist the loop! So there is a cycle
		fast = head;
		// Let the fast pointer starts from the head
		// When slow and fast meets again, this is the postion of the loop.
		while (fast != slow) {
			fast = fast->next;
			slow = slow->next;
		}
		return fast;
	}
};
// @lc code=end
