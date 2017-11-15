#!/bin/sh

test=$1
board=$2
msg=$3
user=$4

echo "$board $msg!" | mail -s "$test start!" $user

echo "bash sent email"

exit 0

