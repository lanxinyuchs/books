#!/bin/sh

type=$1
infile=$2
outdir=$3
mnesia_log=""
tmp_dir=/tmp/${USER}_dumpcert


usage() {
  echo "dumpcert.sh -dcg|-esi|-log <input>  <outdir>"
  exit 1
}

cleanup() {
 if [ -d $tmp_dir ]; then
   echo "Cleaning $tmp_dir"
   rm -Rf $tmp_dir
 fi

}

quit() {
 cleanup
 exit 1
}

init() {
 if [ ! -d $tmp_dir ]; then
     echo "create tmp dir $tmp_dir"
     mkdir $tmp_dir
 fi

 if [ ! -d $outdir ]; then
     echo "create outdir $outdir"
     mkdir $outdir
 fi


}

extract_log_from_dcg() {
 echo "Extracting ESI from DCG"
 dcg=$1
 unzip -qo $dcg *logfiles.zip -d $tmp_dir
 unzip -qoj $tmp_dir/*logfiles.zip rcslogs/esi* -d $tmp_dir
 esi=`ls -1 $tmp_dir/esi*`
 if [ ${esi: -4} == ".gpg" ]; then
     echo "Found encrypted ESI. Extract ESI manually, decrypt it and use -esi flag"
     quit
 fi
 extract_log_from_esi $esi
 
}

extract_log_from_esi() {
 echo "Extracting log from ESI"
  esi=$1
  cp -f $esi $tmp_dir 2>/dev/null
  gunzip $tmp_dir/*.gz
  tar xvf $tmp_dir/*.tar -C $tmp_dir rcs/db/mnesia/mnesia_esi.log
  mnesia_log=$tmp_dir/rcs/db/mnesia/mnesia_esi.log
}

if [ $# -lt 3 ]; then
  usage
fi

init

if [ $type == "-log" ]; then
   mnesia_log=$infile
   echo "Using provided log file"
elif [ $type == "-esi" ]; then
  extract_log_from_esi $infile
elif [ $type == "-dcg" ]; then
  extract_log_from_dcg $infile
else
  usage
fi




# mnesia DB contains pm attributes with Pids e.g <0.4444.0> and OTP does not support
# importing file with PIDs and References
# substitute them with undefined

echo "Cleaning  $mnesia_log from process entries"

cat $mnesia_log | sed -r 's/<[[:digit:]]+[[:punct:]][[:digit:]]+[[:punct:]][[:digit:]]>/undefined/g' > $tmp_dir/mnesia.log

# The grep is just to cut out some debug printouts from mnesia
escript dumpcert.erl $tmp_dir/mnesia.log $outdir | grep -v -i "table"

cleanup




