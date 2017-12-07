#!/bin/sh
# ----------------------------------------------------------------------
# Author:       etxbjca
#
# Short description: Extract metadata from Office documents.
#                    
#
# **********************************************************************
#
# %CCaseCopyrightBegin%
# Copyright (c) Ericsson AB 2014-2017 All rights reserved.
# 
# The information in this document is the property of Ericsson.
# 
# Except as specifically authorized in writing by Ericsson, the 
# receiver of this document shall keep the information contained 
# herein confidential and shall protect the same in whole or in 
# part from disclosure and dissemination to third parties.
# 
# Disclosure and disseminations to the receivers employees shall 
# only be made on a strict need to know basis.
# %CCaseCopyrightEnd%
#
# **********************************************************************
#

if [ -n "${DEBUG}" ]
then
     set -x
     debug='-d'
fi
#
# Define variables
#
progname="`basename $0`"
progdir="`dirname $0`"
ArcInit="/app/modules/0/init/sh"
tmpdir=`mktemp -d`
tmp=${tmpdir}/${progname}.tmp
tmp2=${tmpdir}/${progname}.tmp2
tmp3=${tmpdir}/${progname}.tmp3
Exit=0

#
# trap handler: cleanup in case of error
#
function trap_handler()
{
    LASTERR=$2
    if [ -z "${debug}" ]
    then
	\rm -rf ${tmpdir}
    fi
    exit $LASTERR
}

# Clean on abort
trap 'trap_handler ${LINENO} $?' 0 2 3 15

if [ -n "${DISPLAY}" ]
then
    storeopt=''
else
    if xvfb_proc=`pgrep -u $USER Xvfb`
    then
	export DISPLAY=`ps -fp ${xvfb_proc} | grep Xvfb | awk '{print $9}'`
    else
	display=":`id -u`"
	if Xvfb $display -shmem > /dev/null 2>&1 &
	then
	    export DISPLAY=${display}
	else
	    Error "Faild to Start Fake Display!"
	fi
    fi
    storeopt='-N'
fi

# Source common procedures for PDI support
. ${progdir}/2source/pdiSupport.sh


#
# Define Procedure for Word Documents
#
ExtractWORDdoc()
  {
      case ${file##*.} in
	  docm)	unzip -p "${file}" | grep --text '<w:r' | sed -e 's/<w:p[^<\/]*>/ /g' -e 's/<[^<]*>//g' | grep -v '^[[:space:]]*$' | sed G |  tr '[:cntrl:]' '\n' > ${tmp3}
		grep 'DOCPROPERTY' ${tmp3} | sort -u | tr -d '"'| \
		sed -e 's/^[ ]*DOCPROPERTY[ ]*//g' \
		    -e 's/[ ]*DOCPROPERTY[ ]*/\n/g' \
		    -e 's/[ ]+.*MERGEFORMAT[ ]*/=/g' \
		    -e 's/[ \*]*MERGEFORMAT/=/g' \
		    -e 's/LangCode//g' \
		    -e 's/[ ]*=[ ]*/="/g' \
		    -e 's/ Rev//g' \
		    -e 's/[ ]*PAGE[ ]*.*$//g' | \
		sort -u | sed -e 's/[[:space:]]*$/"/g' > ${tmp2}
		grep 'TITLE' ${tmp3}| \
		head -1 | \
		tr -d '\\' | \
		sed -e 's/[[:space:]]*$//g' \
		    -e 's/^[[:space:]]*//g' \
		      -e 's/^[[:punct:]]*//g' \
		    -e 's/[\$\[\$]*//g' \
		    -e 's/^TITLE[[:space:]]*\(.*\)$/Title="\1"/g'| \
		grep '=' | uniq >> ${tmp2}
		if ! grep 'Title=' ${tmp2} > /dev/null 2>&1
		then
		  grep -A 1 -B 1 Contents ${tmp3}| \
		  head -1 | \
		  sed -e 's/[[:space:]]*$//g' \
		      -e 's/^[[:space:]]*//g' \
		      -e 's/^[[:punct:]]*//g' \
		      -e 's/^\(.*\)$/Title="\1"/g'| \
		  grep '=' | uniq >> ${tmp2}
		fi
	  	sort -u ${tmp2} > ${tmp};;
	  docx)	libreoffice --headless -convert-to doc ${file} -outdir ${tmpdir} > /dev/null 2>&1
	  	catdocfile=${tmpdir}/`basename ${file%.docx}`.doc
		catdoc -wxt -d windows-1252 ${catdocfile} | \
		grep 'DOCPROPERTY' | \
		tr -s '&' '\n' | \
		tr -d '\\' | \
		sed -e 's/[\$\[\$]*//g' \
		    -e 's/^.*DOCPROPERTY[[:space:],"]*//g' \
		    -e 's/" /="/g' \
		    -e '/^$/d' \
		    -e 's/\(.*\)$/\1"/g' \
		    -e 's/[[:space:]][uU][eE][nN]/ Uen/g' \
		    -e 's/[[:space:]]*"$/"/g'| \
		grep '=' | uniq > ${tmp}
		catdoc -wxt -d windows-1252 ${catdocfile} | \
		grep 'TITLE' | \
		tr '[:cntrl:]' '\n' | \
		head -1 | \
		tr -d '\\' | \
		sed -e 's/[[:space:]]*$//g' \
		    -e 's/^[[:space:]]*//g' \
		      -e 's/^[[:punct:]]*//g' \
		    -e 's/[\$\[\$]*//g' \
		    -e 's/^TITLE[[:space:]]*\(.*\)$/Title="\1"/g'| \
		grep '=' | uniq >> ${tmp}
		if ! grep 'Title=' ${tmp} > /dev/null 2>&1
		then
		  catdoc -wxt -d windows-1252 ${catdocfile} | \
		  grep -A 1 -B 1 Contents | \
		  head -1 | \
		  sed -e 's/[[:space:]]*$//g' \
		      -e 's/^[[:space:]]*//g' \
		      -e 's/^[[:punct:]]*//g' \
		      -e 's/^\(.*\)$/Title="\1"/g'| \
		  grep '=' | uniq >> ${tmp}
		fi;;
	  doc)	catdocfile=${file}
  		catdoc -wxt -d windows-1252 ${catdocfile} | \
		grep DOCPROPERTY | \
		tr -s '&' '\n' | \
		tr '[:cntrl:]' '\n' | \
		tr -d '"' | \
		sed -e 's/[\$\[\$]*//g' \
		    -e 's/^.*DOCPROPERTY[[:space:],"]*//g' \
		    -e 's/[[:space:]].*MERGEFORMAT[[:space:]][[:space:]]*\(.*\)/="\1"/g' \
		    -e 's/[[:space:]][[:space:]]*"/"/g' \
		    -e 's/[ ]*LangCode//g' | \
		grep '=' | uniq > ${tmp};;
	  *)	Error "Unknown file type [${file}]";;
      esac
      . ${tmp}
}

#
# Define Procedure for PPT Documents
#
ExtractPPTdoc()
  {
      Prepared=''
      Title=''
      filename=`basename ${file%.*}`
      libreoffice --headless --convert-to pdf ${file} --outdir ${tmpdir} > /dev/null 2>&1 || Error ""
      pdftotext -f 2 -l 2 -nopgbrk ${tmpdir}/${filename}.pdf - | grep -v '^$' | sed -e 's/ Rev:/\nRev:/' -e 's/ Date:/\nDate:/' > ${tmpdir}/${filename}.txt
      if grep '|.*, Rev' ${tmpdir}/${filename}.txt
      then
	  grep '|.*, Rev' ${tmpdir}/${filename}.txt | sed 's/| /|/g' > ${tmpdir}/${filename}.txt2
	  Date=`awk -F\| '{print $4}' ${tmpdir}/${filename}.txt2`
	  DocNo=`awk -F\| '{print $3}' ${tmpdir}/${filename}.txt2| awk -F, '{print $1}'`
	  Revision=`awk -F\| '{print $3}' ${tmpdir}/${filename}.txt2 | awk -F, '{print $2}'| awk '{print $2}'`
	  Title=`awk -F\| '{print $1}' ${tmpdir}/${filename}.txt2 | sed 's/[ ]*$//g'`
      else
	  Date=`grep 'Date:' ${tmpdir}/${filename}.txt | awk '{print $2}'`
	  if [ -z "${Date}" ]
	  then
	      Date=`tail -1 ${tmpdir}/${filename}.txt | awk -F\| '{print $4}' | tr -d '[:space:]'`
	  fi
	  DocNo=`grep 'No:' ${tmpdir}/${filename}.txt | sed 's/^No: //'`
	  if [ -z "${DocNo}" ]
	  then
	      DocNo=`tail -1 ${tmpdir}/${filename}.txt | awk -F\| '{print $3}' | awk -F, '{print $1}' | sed -e 's/^ *//' -e 's/ *$//'`
	  fi
	  Revision=`grep 'Rev:' ${tmpdir}/${filename}.txt | awk '{print $2}'`
	  Title=`tail -1 ${tmpdir}/${filename}.txt | sed 's/ [A-Z]* [eE][rR][iI][cC][sS][sS][oO][nN].*//g'`
	  if [ -z "${Revision}" ]
	  then
	      Revision=`tail -1 ${tmpdir}/${filename}.txt | awk -F\| '{print $3}' | awk -F, '{print $2}' | awk '{print $2}'`
	      Title=`tail -1 ${tmpdir}/${filename}.txt | awk -F\| '{print $1}'`
	  fi
      fi
}

#
# Define Procedure for Excel Documents
#
ExtractXLSdoc()
  {
      Prepared=''
      Title=''
      filename=`basename ${file%.*}`
      libreoffice --headless --convert-to pdf ${file} --outdir ${tmpdir} > /dev/null 2>&1 || Error ""
      Date=`pdftotext -f 2 -l 2 -nopgbrk ${tmpdir}/${filename}.pdf - | sed 's/_x000D_/\n/g' | grep 'Date:' | awk '{print $2}'` 2> /dev/null || Error ""
      DocNo=`pdftotext -f 2 -l 2 -nopgbrk ${tmpdir}/${filename}.pdf - | sed 's/_x000D_/\n/g' | grep 'No:' | sed 's/^.*No:[[:space:]]*//g'` 2> /dev/null || Error ""
      Revision=`pdftotext -f 2 -l 2 -nopgbrk ${tmpdir}/${filename}.pdf - | sed 's/_x000D_/\n/g' | grep 'Rev:' | awk '{print $2}'` 2> /dev/null || Error ""
      Title=`pdftotext -f 2 -l 2 -nopgbrk ${tmpdir}/${filename}.pdf - | sed 's/_x000D_/\n/g' | grep '^[A-Z][A-Z][A-Z][A-Z]' | sed -e 's/^[[:upper:]]*[[:space:]]*[[:upper:]]*[[:space:]]*[[:upper:]]*[[:space:]]*//g' -e 's/[ ]*$//g'` 2> /dev/null || Error ""
      if [ -z "${Title}" ]
      then
	  Title=`pdftotext -f 2 -l 2 -nopgbrk ${tmpdir}/${filename}.pdf - | sed 's/_x000D_/\n/g' | sed 's/_x000D_/\n/g' | sed -n '2,5p' | tr '\n' ' ' | sed 's/[ ]*$//g'` 2> /dev/null || Error ""
      fi
}

ExtractXLSXdoc()
  {
      Prepared=''
      filename=`basename ${file%.*}`
      libreoffice --headless --convert-to csv ${file} --outdir ${tmpdir} > /dev/null  2>&1 || Error ""
      Date=`grep 'Date:' ${tmpdir}/${filename}.csv | awk '{print $2}'` 2> /dev/null || Error ""
      DocNo=`grep 'Doc Number:' ${tmpdir}/${filename}.csv | awk -F: '{print $2}' | tr -d ',"'` 2> /dev/null || Error ""
      Revision=`grep 'Revision:' ${tmpdir}/${filename}.csv | awk -F: '{print $2}' | tr -d ',"'` 2> /dev/null || Error ""
      Title=`sed -ne '1,6p' ${tmpdir}/${filename}.csv | tr -d '\n' | sed 's/^[\,, ]*//' | tr -d ','` 2> /dev/null || Error ""
}


#
# Define Procedure for PDF Documents
#
ExtractPDFdoc()
  {
      Prepared=''
      Title=''
      Date=`pdftotext -f 2 -l 2 ${file} - | grep 'Date:' | awk '{print $2}'` 2> /dev/null 2>&1 || Error ""
      DocNo=`pdftotext -f 2 -l 2 -nopgbrk ${file} - | grep 'No:' | sed 's/^.*No:[[:space:]]*//g'` 2> /dev/null || Error ""
      Revision=`pdftotext -f 2 -l 2 -nopgbrk ${file} - | grep 'Rev:' | awk '{print $2}'` 2> /dev/null || Error ""
      Title=`pdftotext -f 2 -l 2 -nopgbrk ${file} -| grep '^[A-Z][A-Z][A-Z][A-Z]' | sed 's/^[[:upper:]]*[[:space:]]*[[:upper:]]*[[:space:]]*[[:upper:]]*[[:space:]]*//g'` 2> /dev/null || Error ""
}


#
# Define Procedure for CDF V2 Documents
#
ExtractCDFdoc()
  {
      fileInfo=`file "${file}"`
      Date=''
      Prepared=`echo ${fileInfo} | tr ',' '\n' | grep 'Author:' | awk -F: '{print $2}' | sed 's/^[ ]*//'` 2> /dev/null 2>&1 || Error ""
      DocNo=`echo ${fileInfo} | tr ',' '\n' | grep 'Comments:' | awk -F: '{print $2}' | sed 's/[A-Z,a-z]Rev/Rev/g' | awk -F'Rev' '{print $1}' | sed -e 's/^[ ]*//' -e 's/[ ]*$//'` 2> /dev/null || Error ""
      Revision=`echo ${fileInfo} | tr ',' '\n' | grep 'Comments:' | awk -F: '{print $2}' | sed 's/[A-Z,a-z]Rev/Rev/' | awk -F'Rev' '{print $2}' | sed -e 's/^[ ]*//' -e 's/[ ]*$//'` 2> /dev/null || Error ""
      Title=`echo ${fileInfo} | tr ',' '\n' | grep 'Title:' | awk -F: '{print $2}' | sed 's/^[ ]*//'` 2> /dev/null 2>&1 || Error ""
}

#
# Here starts the executing
#

file="$*"

# Get File Type and define GASK storage parameters
eltype=`file "${file}" | awk -F: '{print $2}' | awk -F, '{print $1}' | sed 's/^[\t, ]*//g'`

#
# Check if file exist
#
if [ ! -s "${file}" ]
then
    Error "Missing/empty file [${file}]"
fi


#
# Initate ARC environment
#
if [ -r "${ArcInit}" ]
then
    . ${ArcInit}
   if ! module add catdoc/0.95
   then
       Error 'Module add of catdoc failed!'
   fi
fi

case "${eltype}" in
    *'CDF V2 Document'*)
    			 if [ -z "${DocNo}" -o -z "${Revision}" ]
			 then
			     case ${file##*.} in
				    doc*)	ExtractWORDdoc|| Error "Extraction of Word document header data failed for file [${file}]!";;
				     pdf)	ExtractPDFdoc || Error "Extraction of PDF document header data failed for file [${file}]!";;
				ppt|pptx)	ExtractPPTdoc || Error "Extraction of PPT document header data failed for file [${file}]!";;
				xls|xlsx)	ExtractXLSdoc || Error "Extraction of Excel document header data failed for file [${file}]!";;
				       *)	Error "Unsupported file [${file}] type (only WORD/PPT supported)!";;
			     esac
			 else
			     ExtractCDFdoc || Error "Extraction of CDF V2 Document header data failed for file [${file}]!"
			 fi;;
    *'Microsoft Word'*)	 ExtractWORDdoc || Error "Extraction of WORD document header data failed for file [${file}]!";;
    *'compressed data'*)
    			case ${file##*.} in
				pdf)	ExtractPDFdoc || Error "Extraction of PDF document header data failed for file [${file}]!";;
				ppt|pptx)	ExtractPPTdoc || Error "Extraction of PPT document header data failed for file [${file}]!";;
			    	xls|xlsx)	ExtractXLSdoc || Error "Extraction of Excel document header data failed for file [${file}]!";;
			    	*)	Error "Unsupported file [${file}] type (only WORD/PPT supported)!";;
			esac;;
    *) 			Error "Unknown file type [${file}]!";;
esac

echo 'DocNo="'"${DocNo}"'"'
echo 'Revision="'"${Revision}"'"'
