#!/bin/bash
#
# About: Install built Linux kernel
#

sudo make modules_install
# install SHOULD creat initramfs and update GRUB automatically in recent kernel versions
# So, update-intramfs and update-grub commands are not needed anymore
sudo make install

sudo update-intramfs -c -k all
sudo update-grub
