#!/bin/bash

find ./ -name "*.o" -print -delete
find ./ -name "*.out" -print -delete

find ./ -name "target" -type d -print | xargs rm -r
