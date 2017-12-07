#!/usr/bin/env bash

for remote in $(git remote -v | egrep '(push)' | awk '{print $1}')
do
  git remote set-url $remote --push ssh://gerrit.ericsson.se:29418/rcsmw/rcsmw
done
