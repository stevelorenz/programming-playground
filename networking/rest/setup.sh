#!/bin/bash
#
# setup.sh
#

sudo apt-get update -y
sudo apt-get install -y cmake meson python3-pip

pip3 install --user flask requests
