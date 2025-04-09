#!/bin/bash

echo "check ......"

if [ ! -e /usr/share/bochs/keymaps/x11-pc-us.map ]; then
	echo "/usr/share/bochs/keymaps/x11-pc-us.map does not exist..."
	exit 1
else
	file /usr/share/bochs/keymaps/x11-pc-us.map
fi

if [ ! -e /usr/share/bochs/BIOS-bochs-latest ]; then
	echo " /usr/share/bochs/BIOS-bochs-latest does not exist..."
	exit 1
else
	file /usr/share/bochs/BIOS-bochs-latest
fi

if [ ! -e /usr/share/bochs/VGABIOS-lgpl-latest.bin ]; then
	echo "/usr/share/bochs/VGABIOS-lgpl-latest.bin does not exist..."
	exit 1
else
	file /usr/share/bochs/VGABIOS-lgpl-latest.bin
fi
echo "check over ...."

sleep 2

echo "run........"
/usr/bin/bochs -q -f bochsrc.disk
