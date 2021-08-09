#!/bin/bash
#
# setup.sh
#

apt-get update -y
apt-get install -y cmake meson \
    libgoogle-glog-dev libmsgsl-dev libfmt-dev
