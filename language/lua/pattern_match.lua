#! /usr/bin/env lua
--
-- pattern_match.lua
--

print(string.find("hello world", "hello"))
print(string.match("hello world", "hello"))

dead_line = "Deadline is 02/12/2019"
pattern = "%d%d/%d%d/%d%d%d%d"
print(string.match(dead_line, pattern))

-- [[
-- A lot more, aber, Learn them in the future...
-- ]]
