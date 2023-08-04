#!/bin/bash
#
# About: Use rsync to sync this folder to a Linux VM for testing and debugging.
#        Sometimes, I use MacOS for DEV and sources here are for Linux only, so need to perform build and test on Linux
#

VM_IP="192.168.64.4"

rsync -avz --progress --delete ../the_linux_programming_interface "zuoxiang@${VM_IP}:~"
