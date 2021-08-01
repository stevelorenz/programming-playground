#
# @lc app=leetcode id=232 lang=python3
#
# [232] Implement Queue using Stacks
#

# @lc code=start
class MyQueue:
    def __init__(self):
        """
        Initialize your data structure here.
        """
        self.stack1 = []
        self.stack2 = []

    def push(self, x: int) -> None:
        """
        Push element x to the back of queue.
        """
        # O(1) for push
        self.stack1.append(x)

    def pop(self) -> int:
        """
        Removes the element from in front of queue and returns that element.
        """
        # O(n) for pop
        # Invert the ordering in the list before pop
        if len(self.stack2) <= 0:
            while len(self.stack1):
                curr = self.stack1.pop()
                self.stack2.append(curr)
        res = self.stack2.pop()
        return res

    def peek(self) -> int:
        """
        Get the front element.
        """
        if len(self.stack2) <= 0:
            while len(self.stack1):
                curr = self.stack1.pop()
                self.stack2.append(curr)
        return self.stack2[-1]

    def empty(self) -> bool:
        """
        Returns whether the queue is empty.
        """
        return len(self.stack1) + len(self.stack2) == 0


# Your MyQueue object will be instantiated and called as such:
# obj = MyQueue()
# obj.push(x)
# param_2 = obj.pop()
# param_3 = obj.peek()
# param_4 = obj.empty()
# @lc code=end
