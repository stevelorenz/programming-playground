#!/bin/bash

xauth add $(xauth -f /home/"$SUDO_USER"/.Xauthority list | tail -1)
