#!/bin/bash

print_size()
{
    echo "Actual(stripped) image size is: $(($1*256**3+$2*256**2+$3*256+$4))" >&2
}

[ $# -lt 1 ] && { 
echo "
USAGE: $0 <input_image_file_path> [<out_image_file_path]

	If <input_image_file_path> contains a KAK signed image its header will
	be stripped and the resulting image is placed in <out_image_file_path>

	out_image_file_path defaults to /tmp/out.img
	setting out_image_file_path to - will cause writing to sdout
	
	The trailer is not stripped away since that isn't needed, but good to 
	know if you compare files.

	If the input doesn't contain a signed image, this will just copy (also
	using dd).
" >&2
exit 1
}

image_file=${1?Need one or two arguments: the image to check/strip, outputfile}
output_file=${2:-/tmp/out.img}

[ "x$output_file" = "x-" ] && output_file=/dev/stdout

[ -r ${image_file} ] || {
    echo Image: $image_file - unreadable >&2
    exit 1
}

set -- $(od  -An -ta -N 4 ${image_file}) || {
    echo Failed to read image: $image_file >&2
    exit 1
}

##echo "Magic: |${*}| |${@}| |${1}|${2}|${3}|${4}|"

case :${1}:${2}:${3}:${4}: in
    :S:B:B:dc1:)
	echo "Image is signed, $image_file -> stripping -> $output_file" >&2
	print_size $(od -An -j 4 -tu1 -N 4 $image_file)
	dd if=$image_file of=$output_file skip=1 bs=140 2>/dev/null || {
	    echo Failed to strip/write >&2
	    exit 1
	}
    ;;
    *)
	echo "Image is not signed, $image_file -> copy -> $output_file" >&2
	dd if=$image_file of=$output_file 2>/dev/null || {
	    echo Failed to copy >&2
	    exit 1
	}
    ;;
esac
exit 0
