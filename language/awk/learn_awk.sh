#!/bin/bash

AWK="gawk"

${AWK} '$3 > 0 { print $1, $2 * $3 }' ./emp.data
echo ""

${AWK} '{ print NR, NF }' ./emp.data
echo ""

${AWK} 'BEGIN { print "NAME RATE HOURS"; print "" } { print NR, NF }' ./emp.data
echo ""

${AWK} '$3 > 15 { emp = emp + 1 } END { print emp, "employees worked more than 15 hours" }' ./emp.data
echo ""


${AWK} '{ print $1, length($1) }' ./emp.data
echo ""
