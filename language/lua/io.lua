#! /usr/bin/env lua
--
-- io.lua
-- About: I/O in Lua
--

io.write(math.sin(math.pi / 3), "\n")
io.write(string.format("%.4f\n", math.sin(math.pi / 3)))

io.input("./test.txt")

for line in io.lines() do
    io.write(string.format("%s---", line))
end
io.write("\n")

-- [[
-- The complete I/O model
-- ]]

local f = assert(io.open("./test.txt"), "r")
--- Read all bytes.
local t = f:read("*a")
io.write(string.format(t))
f:close()
