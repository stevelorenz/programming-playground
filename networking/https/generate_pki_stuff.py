#! /usr/bin/env python3
# -*- coding: utf-8 -*-
# vim:fenc=utf-8

from cryptography import x509
from cryptography.hazmat.backends import default_backend
from cryptography.hazmat.primitives import serialization
from getpass import getpass

import pki_helpers

private_key = pki_helpers.generate_private_key("ca-private-key.pem", "foo")
pki_helpers.generate_public_key(
    private_key,
    filename="ca-public-key.pem",
    country="US",
    state="Maryland",
    locality="Baltimore",
    org="My CA Company",
    hostname="my-ca.com",
)

server_private_key = pki_helpers.generate_private_key("server-private-key.pem", "bar")

pki_helpers.generate_csr(
    server_private_key,
    filename="server-csr.pem",
    country="US",
    state="Maryland",
    locality="Baltimore",
    org="My Company",
    alt_names=["localhost"],
    hostname="my-site.com",
)

with open("server-csr.pem", "rb") as csr_file:
    csr = x509.load_pem_x509_csr(csr_file.read(), default_backend())

with open("ca-public-key.pem", "rb") as ca_public_key_file:
    ca_public_key = x509.load_pem_x509_certificate(
        ca_public_key_file.read(), default_backend()
    )
with open("ca-private-key.pem", "rb") as ca_private_key_file:
    ca_private_key = serialization.load_pem_private_key(
        ca_private_key_file.read(),
        getpass().encode("utf-8"),
        default_backend(),
    )

pki_helpers.sign_csr(csr, ca_public_key, ca_private_key, "server-public-key.pem")
