#!/bin/bash

echo "Run Jenkins in a Docker container"
docker run --name jenkins --rm -p 8080:8080 jenkins
