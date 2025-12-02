#!/bin/bash

arch="$(uname -p)"

if [ "$arch" = "aarch64" ]; then
	arch="arm64"
fi

sudo lxc-destroy --name mycontainer
sudo time lxc-create --name mycontainer --template download -- --dist ubuntu --release jammy --arch "$arch"
sudo lxc-start --name mycontainer
sudo lxc-info --name mycontainer
sudo lxc-ls --fancy
sudo lxc-attach --name mycontainer
sudo lxc-stop --name mycontainer
sudo lxc-destroy --name mycontainer
sudo lxc-ls --fancy
