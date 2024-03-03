#!/bin/bash

# Delete found executable files recursively
find ./ -type f -perm +111 -delete

echo "Cleaning completed."
