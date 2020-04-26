#!/bin/bash

cd $HOME || exit
git clone https://github.com/torvalds/linux
cd ./linux || exit
git checkout v5.3
