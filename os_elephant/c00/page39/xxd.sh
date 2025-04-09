#!/bin/bash

#
# -u: Use upper case
# -a: Toggle auto-skip
# -g: Number of octets in each group
# -s: Seek at <seek> bytes
# -l: Stop after <l> bytes

xxd -u -a -g 1 -s $2 -l $3 $1
