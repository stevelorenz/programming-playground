#!/bin/bash
#
# setup.sh
#

apt-get update -y
apt-get install -y cmake meson \
    libboost-dev libboost-program-options-dev libasio-dev libboost-system-dev libboost-coroutine-dev libboost-log-dev \
    libmsgsl-dev icu-devtools
