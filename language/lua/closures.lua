#! /usr/bin/env lua
--
-- closures.lua
--

local dump = require 'pl.pretty'.dump

local network = {
    {name = "grauna", IP = "210.26.30.34"},
    {name = "arraial", IP = "210.26.30.23"},
    {name = "lua", IP = "210.26.23.12"},
    {name = "derain", IP = "210.26.23.20"},
}

dump(network)
table.sort(network, function (a, b) return (a.name > b.name) end)
print("* Table after sort:")
dump(network)

Lib = {}
Lib.foo = function (a, b) return (a + b) end
Lib.goo = function (a, b) return (a - b) end
print(Lib.foo(1, 2))

-- [[
-- Closure
-- Closure is a method to bound a method/function to some given data (upvalues).
-- ]]

function NewCounter()
    local i = 0
    return function ()
        i = i + 1
        return i
    end
end

c1 = NewCounter()
c2 = NewCounter()
print(c1())
print(c2())
print(c1())

do
    oldSin = math.sin
    math.sin = function (x) return x * 2 end
    print( math.sin(3) )
    math.sin = oldSin
end
print(math.sin(3))

-- [[
-- Functional Programming
-- ]]

function disk(cx, cy, r)
    return function(x, y)
        return (x - cx)^2 + (y - cy)^2 <= r^2
    end
end
print(disk(1.0, 3.0, 4.5))
