#
# @lc app=leetcode id=225 lang=python3
#
# [225] Implement Stack using Queues
#

# @lc code=start
class MyStack:
    def __init__(self):
        """
        Initialize your data structure here.
        """
        self.queue1 = []

    def push(self, x: int) -> None:
        """
        Push element x onto stack.
        """
        # O(1) for the push operation
        self.queue1.append(x)

    def pop(self) -> int:
        """
        Removes the element on top of the stack and returns that element.
        """
        # O(n) for the pop operation
        queue2 = []
        tmp = len(self.queue1)
        for _ in range(tmp - 1):
            queue2.append(self.queue1.pop(0))
        # Get the last element in the queue1
        x = self.queue1.pop()
        self.queue1 = queue2
        return x

    def top(self) -> int:
        """
        Get the top element.
        """
        return self.queue1[-1]

    def empty(self) -> bool:
        """
        Returns whether the stack is empty.
        """
        return len(self.queue1) == 0


# Your MyStack object will be instantiated and called as such:
# obj = MyStack()
# obj.push(x)
# param_2 = obj.pop()
# param_3 = obj.top()
# param_4 = obj.empty()
# @lc code=end
