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

$bootstrap= <<-SCRIPT
DEBIAN_FRONTEND=noninteractive apt-get update
DEBIAN_FRONTEND=noninteractive apt-get upgrade -y
APT_PKGS=(
  ansible
  python3-pip
)
DEBIAN_FRONTEND=noninteractive apt-get install -y --no-install-recommends "${APT_PKGS[@]}"
SCRIPT

Vagrant.configure("2") do |config|

  config.vm.define "playground" do |playground|
    playground.vm.box = "bento/ubuntu-20.04"
    playground.vm.hostname = "playground"
    playground.vm.provision :shell, inline: $bootstrap

    playground.vm.provision "ansible_local" do |ansible|
      ansible.playbook = "./playbooks/bootstrap.yml"
      ansible.install = false
    end

    playground.vm.provider "virtualbox" do |vb|
      vb.name = "playground"
      vb.memory = RAM
      vb.cpus = CPUS
    end
  end
end
