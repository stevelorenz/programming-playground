# -*- mode: ruby -*-
# vi: set ft=ruby :

###############
#  Variables  #
###############

CPUS = 4
RAM = 4096
DISK_SIZE = "60GB"

BOX = "bento/ubuntu-22.04"

#######################
#  Provision Scripts  #
#######################

$bootstrap= <<-SCRIPT
DEBIAN_FRONTEND=noninteractive apt-get update
DEBIAN_FRONTEND=noninteractive apt-get upgrade -y
APT_PKGS=(
  ansible
  cloud-guest-utils
  lvm2
  python3-pip
)
DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends "${APT_PKGS[@]}"

# Extend the root file system to the resized the disk size: DISK_SIZE
growpart /dev/sda 3
partprobe /dev/sda
pvresize /dev/sda3
lvextend -l +100%FREE /dev/ubuntu-vg/ubuntu-lv
resize2fs /dev/mapper/ubuntu--vg-ubuntu--lv
SCRIPT

#################
#  Main Config  #
#################

Vagrant.configure("2") do |config|

  config.vm.define "playground" do |playground|
    playground.vm.box = BOX
    playground.vm.disk :disk, size: DISK_SIZE, primary: true
    playground.vm.hostname = "playground"

    playground.vm.provider "virtualbox" do |vb|
      vb.memory = RAM
      vb.cpus = CPUS
    end

    playground.vm.provision :shell, inline: $bootstrap
    playground.vm.provision "ansible_local" do |ansible|
      ansible.playbook = "./ansible/bootstrap.yml"
      ansible.install = false
    end
    playground.vm.provision "ansible_local" do |ansible|
      ansible.playbook = "./ansible/docker.yml"
      ansible.install = false
    end

    playground.ssh.forward_agent = true
    playground.ssh.forward_x11 = true

  end
end
