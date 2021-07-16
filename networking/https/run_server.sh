#!/bin/bash
#
# run_server.sh
#
#

python3 ./generate_pki_stuff.py
~/.local/bin/uwsgi --master --https localhost:5000,server-public-key.pem,server-private-key.pem --mount /=server:app
