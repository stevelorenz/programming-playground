#!/bin/bash

if [ -e "./hd.img" ]; then
	rm ./hd.img
fi

bximage -q -func=create -hd=60M hd.img
