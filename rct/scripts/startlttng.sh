#!/bin/sh 

if [ $1 = relayd ] ; then 
. /usr/share/modules/init/sh
module use /app/rbs/modules
module add wrhosttools

echo "rm -rf lttng-trace"
rm -rf lttng-trace
echo "lttng-relayd -C tcp://0.0.0.0:5342 -D tcp://0.0.0.0:5343 -L net://localhost:5344 -o $PWD/lttng-trace/ &"
lttng-relayd -C tcp://0.0.0.0:5342 -D tcp://0.0.0.0:5343 -L net://localhost:5344 -o $PWD/lttng-trace/ &
pwdbabel=$PWD
echo "\n The present working directory is $pwdbabel"
fi

if [ $1 = babel ] ; then 
echo "babeltrace $PWD/lttng-trace/du1/$2"
babeltrace $PWD/lttng-trace/du1/$2
fi
