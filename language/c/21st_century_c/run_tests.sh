#!/bin/bash
#
# run_tests.sh
#

ctest --repeat until-fail:5 --output-on-failure
