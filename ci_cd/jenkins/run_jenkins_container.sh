#!/bin/bash

echo "* Run Jenkins in a Docker container listening on port 8080"
docker run --name jenkins --rm -p 8080:8080 jenkins
