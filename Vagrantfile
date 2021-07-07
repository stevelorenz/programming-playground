# -*- mode: ruby -*-
# vi: set ft=ruby :

###############
#  Variables  #
###############

CPUS = 4
RAM = 2048

#######################
#  Provision Scripts  #
#######################

$bootstrap_ubuntu= <<-SCRIPT
# Install dependencies
apt-get update -qq
DEBIAN_FRONTEND=noninteractive apt-get install -y sudo git pkg-config gdb bash-completion htop dfc \
    build-essential autoconf libncurses5-dev \
    libssl-dev bison flex libelf-dev linux-headers-$(uname -r)
SCRIPT

$bootstrap_devtools= <<-SCRIPT
apt-get update -qq
DEBIAN_FRONTEND=noninteractive apt-get install -y rustc golang
SCRIPT

Vagrant.configure("2") do |config|

  config.vm.define "playground" do |playground|
    playground.vm.box = "bento/ubuntu-20.04"
    playground.vm.hostname = "playground"
    playground.vm.provision :shell, inline: $bootstrap_ubuntu
    playground.vm.provision :shell, inline: $bootstrap_devtools

    # VirtualBox-specific configuration
    playground.vm.provider "virtualbox" do |vb|
      vb.name = "playground"
      vb.memory = RAM
      vb.cpus = CPUS
    end
  end
end
