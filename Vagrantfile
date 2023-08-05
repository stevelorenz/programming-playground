# -*- mode: ruby -*-
# vi: set ft=ruby :

###############
#  Variables  #
###############

CPUS = 4
RAM = 4096

BOX = "bento/ubuntu-22.04"
BOX_LIBVIRT = "generic/ubuntu2204"

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

$setup_libvirt_vm_always= <<-SCRIPT
# Configure the SSH server to allow X11 forwarding with sudo
cp /vagrant/comnetsemu/util/sshd_config /etc/ssh/sshd_config
systemctl restart sshd.service
SCRIPT

#################
#  Main Config  #
#################

Vagrant.configure("2") do |config|

  config.vm.define "playground" do |playground|
    playground.vm.box = BOX
    playground.vm.hostname = "playground"

    playground.vm.provider "virtualbox" do |vb|
      vb.memory = RAM
      vb.cpus = CPUS
    end

    playground.vm.provider "libvirt" do |libvirt, override|
      override.vm.box = BOX_LIBVIRT
      override.vm.synced_folder ".", "/vagrant", type: "nfs", nfs_version: 4

      libvirt.driver = "kvm"
      libvirt.cpus = CPUS
      libvirt.memory = RAM
    end

    playground.vm.provision :shell, inline: $bootstrap
    playground.vm.provision "ansible_local" do |ansible|
      ansible.playbook = "./ansible/bootstrap.yml"
      ansible.install = false
    end
    playground.vm.provider "libvirt" do |libvirt, override|
      override.vm.provision :shell, inline: $setup_libvirt_vm_always, privileged: true, run: "always"
    end

    playground.ssh.forward_agent = true
    playground.ssh.forward_x11 = true

  end
end
