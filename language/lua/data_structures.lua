#! /usr/bin/env lua
--
-- data_structures.lua
--


-- String buffers
-- Use table as string buffers to avoid the copy of immutable strings.
io.input("./test.txt")
local buf = {}
for line in io.lines() do
    buf[#buf + 1] = line .. "\n"
end
local s = table.concat(buf)
print(s)
