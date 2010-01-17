#!/bin/sh

# this script takes an input directory and removes all comments
# // and /* */ (without nested comments) of all files with a given
# file extension (see parameters below)

# parameters:
# cmd - script name itself
# indir - input-directory
# ext - extension of files being processed

cmd=${0}

if [ -z ${1} ]; then
	echo 'ERROR: no input directory given (absolute path necessary)! terminating'
	exit -1
fi
indir=${1}

if [ -z ${2} ]; then
	echo 'ERROR: no extension given! terminating'
	exit -1
fi
ext=${2}

# change to the script directory
if [ `dirname ${cmd}` != '.' ]; then
	cd `dirname ${cmd}` || exit -1
fi

echo 'INFO: prepraring source files ...'
echo 'INFO: copying all files from the input directory to the output'
echo '      directory and remove all comments'

# create xml representation according to the given extension
# C
if [ ${ext} = 'c' ]; then
	for j in .h .c;
	do
		for i in `find ${indir} -type f -iname "*${j}"`;
		do
			echo 'INFO: processing ' ${i}
			DIR=`dirname ${i}`
			${PWD}/src2srcml --language=C ${i} ${i}.xml
		done
	done
fi

# CPP
if [ ${ext} = 'cpp' ]; then
	for j in .h .cpp;
	do
		for i in `find ${indir} -type f -iname "*${j}"`;
		do
			echo 'INFO: processing ' ${i}
			DIR=`dirname ${i}`
			${PWD}/src2srcml ${i} ${i}.xml
		done
	done
fi

# Java
if [ ${ext} = 'java' ]; then
	for i in `find ${indir} -type f -iname "*.java"`;
	do
		echo 'INFO: processing ' ${i}
		DIR=`dirname ${i}`
		./src2srcml ${i} ${i}.xml
	done
fi


# delete all comments in the xml representations and transfer back to code
for i in `find ${indir} -type f -iname "*.xml"`;
do
	file=`dirname ${i}`/`basename ${i} .xml`
	xsltproc ${PWD}/delete_comments.xsl ${file}.xml > ${file}.xml2
	${PWD}/srcml2src ${file}.xml2 ${file}
	rm ${file}.xml ${file}.xml2
done
