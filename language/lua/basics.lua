#! /usr/bin/env lua
--
-- basics.lua
-- About: Try Lua basics


-- [[
-- Function to calculate the factorial.
-- ]]
function fact(n)
    if n == 1 then
        return 1
    else
        return n * fact(n-1)
    end
end

a = 5
print(fact(a))

a = 1
b = 3
-- Get the maximal number of two numbers
c = (a > b) and a or b
print(c)

-- [[
-- Numbers
-- ]]
print(3/2)
print(3//2)
print(5 % 2)
print(math.sin(math.pi / 6))
math.randomseed(os.time())
print(math.random(1, 6))
print(math.maxinteger+1 == math.mininteger)

-- [[
-- String
-- ]]
a = "one string"
b = string.gsub(a, "one", "another")
print(b)
print(#b)
print(string.len(b))
print(string.format("The length of string b is %u.", #b))
s = "[in brackets]"
print(string.sub(s, 2, -2))

-- [[
-- Tables
-- ]]
t = {}
t["x"] = 10
print(t)
a = {}
for i=1,10,1 do
    a[i] = i*2
end
for i=1,#a do
    print(a[i])
end
for k,v in pairs(a) do
    print(k,v)
end

-- [[
-- Functions
-- ]]

-- Variadic function
function add(...)
    local s = 0
    for _, v in ipairs{...} do
        s = s + v
    end
    return s
end
print(add(3, 4, 10, 25, 12))

print(table.unpack({10, 20, 30}))
f = string.find
a = {"hello", "ll"}
print(f(table.unpack(a)))

function unpack_lua(t, i, n)
    i = i or 1
    n = n or #t
    if i <= n then
        return t[i], unpack(t, i+1, n)
    end
end
