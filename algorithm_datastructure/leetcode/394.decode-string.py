#
# @lc app=leetcode id=394 lang=python3
#
# [394] Decode String
#

# @lc code=start
class Solution:
    def decodeString(self, s: str) -> str:
        cur_num = 0
        cur_str = ""
        stack = []
        for char in s:
            if char == "[":
                stack.append(cur_str)
                stack.append(cur_num)
                cur_num = 0
                cur_str = ""
            elif char == "]":
                pre_num = stack.pop()
                pre_str = stack.pop()
                cur_str = pre_str + pre_num * cur_str
            elif char.isdigit():
                cur_num = cur_num * 10 + int(char)
            else:
                cur_str = cur_str + char
        
        return cur_str


# @lc code=end
