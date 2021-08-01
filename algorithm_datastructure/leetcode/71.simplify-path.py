#
# @lc app=leetcode id=71 lang=python3
#
# [71] Simplify Path
#

# @lc code=start
class Solution:
    def simplifyPath(self, path: str) -> str:
        if path == "":
            return ""

        dirs = path.split("/")
        path_stack = []  # A directory stack
        for d in dirs:
            if d:
                if d == "..":
                    if path_stack:
                        path_stack.pop(-1)
                # Skip dot which is useless
                elif d != ".":
                    path_stack.append(d)
        res = "/" + "/".join(path_stack)
        return res


# @lc code=end
