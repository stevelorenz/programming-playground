#!/bin/bash

echo "# Run QEMU to boot a built Linux kernel for ARM: ./zImage"

QEMU_AUDIO_DRV=none \
    qemu-system-arm -m 256M -nographic -M versatilepb -kernel \
    zImage -append "console=ttyAMA0,115200" -dtb versatile-pb.dtb
