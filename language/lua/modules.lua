#! /usr/bin/env lua
--
-- modules.lua
--

local dump = require 'pl.pretty'.dump
io.write("All loaded modules: \n")
dump(package.loaded)
io.write(string.format("%s\n", package.path))
