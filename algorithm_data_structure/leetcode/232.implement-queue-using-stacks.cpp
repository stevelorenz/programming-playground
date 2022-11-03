/*
 * @lc app=leetcode id=232 lang=cpp
 *
 * [232] Implement Queue using Stacks
 */

// @lc code=start

#include <algorithm>
#include <queue>
#include <stack>
#include <vector>

using namespace std;

class MyQueue {
   public:
	stack<int> in, out;

	MyQueue() {}

	// Put element in in to out -> revert elements in in to out
	void in2out() {
		if (out.empty()) {
			while (not in.empty()) {
				int x = in.top();
				in.pop();
				out.push(x);
			}
		}
	}

	void push(int x) { in.push(x); }

	int pop() {
		in2out();
		int x = out.top();
		out.pop();
		return x;
	}

	int peek() {
		in2out();
		return out.top();
	}

	bool empty() { return in.empty() and out.empty(); }
};

/**
 * Your MyQueue object will be instantiated and called as such:
 * MyQueue* obj = new MyQueue();
 * obj->push(x);
 * int param_2 = obj->pop();
 * int param_3 = obj->peek();
 * bool param_4 = obj->empty();
 */
// @lc code=end
