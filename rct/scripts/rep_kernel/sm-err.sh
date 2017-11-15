#!/bin/sh

test=$1
board=$2
err=$3
user=$4

echo "$board $err!" | mail -s "$test pjut!" $user

echo "bash sent email"

exit 0

