#!/bin/bash

# Make sure you're in the BusyBox source directory
# Run this after `make defconfig`

# Disable crypt-related configs
sed -i -e '/^CONFIG_USE_BB_CRYPT/d' \
    -e '/^CONFIG_FEATURE_SHADOWPASSWDS/d' \
    -e '/^CONFIG_LOGIN/d' \
    -e '/^CONFIG_SU/d' \
    -e '/^CONFIG_PASSWD/d' \
    -e '/^CONFIG_ADDUSER/d' \
    -e '/^CONFIG_DELGROUP/d' \
    -e '/^CONFIG_FEATURE_UTMP/d' \
    -e '/^CONFIG_FEATURE_WTMP/d' \
    -e '/^CONFIG_DATE/d' .config

# Append disables for crypt and date utilities
cat <<EOF >>.config
# Disable crypt-related features
# CONFIG_USE_BB_CRYPT is not set
# CONFIG_FEATURE_SHADOWPASSWDS is not set
# CONFIG_LOGIN is not set
# CONFIG_SU is not set
# CONFIG_PASSWD is not set
# CONFIG_ADDUSER is not set
# CONFIG_DELGROUP is not set
# CONFIG_FEATURE_UTMP is not set
# CONFIG_FEATURE_WTMP is not set

# Disable the date utility
# CONFIG_DATE is not set
EOF

# Apply the updated config
make oldconfig
