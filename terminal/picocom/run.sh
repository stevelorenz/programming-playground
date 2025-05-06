#!/bin/bash

num=200
duration=60
timestamp=$(date +"%Y%m%d_%H%M%S")

echo "# Run ./test_picocom_scal.py with PTY pairs number: ${num} and duration:${duration} seconds. Result file: ./result_${timestamp}.log"

# Use -u option to force the python to run in unbuffered mode
python3 -u ./test_picocom_scal.py --num "${num}" --duration "${duration}" 2>&1 | tee "./result_${timestamp}.log"
