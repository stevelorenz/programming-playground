#!/bin/bash
#
# install_postgis.sh
#

# apt-get update
# apt-get -y install gnupg2
# # Add GPG key
# wget --quiet -O - https://www.postgresql.org/media/keys/ACCC4CF8.asc | apt-key add -
# echo "deb http://apt.postgresql.org/pub/repos/apt/ $(lsb_release -cs)-pgdg main" | tee  /etc/apt/sources.list.d/pgdg.list

apt-get update -y
apt-get install -y postgis postgresql-12-postgis-3
