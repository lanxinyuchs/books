#!/bin/bash -ex

if [ "$#" -ne 2 ] || [ ! -d "$1" ]; then
	echo "Please supply with rcs-test repo absolute path"
	exit 1
fi


if [ ! -d "$2" ]; then
	echo "Please supply with valid whitebox tests absolute path"
	exit 1
fi

echo "Creating links for whitebox tests"

ln -sf "$2/"* "$1/rct/suites/."

echo "Created links for Whitebox tests at $1/rct/suites/"
