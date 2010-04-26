#!/bin/bash

PRJ=`find -type f -name "*Comp.features"`
TARGET=XML
PWD=`pwd`

mkdir $TARGET
	
for I in $PRJ; do
	CUR=`pwd`
	cd `dirname $I`/`basename $I .features`
	doxygen.exe $CUR/Doxyfile
	mkdir -p $CUR/$TARGET/`dirname $I`/`basename $I .features`/
	mv xml $CUR/$TARGET/`dirname $I`/`basename $I .features`/
	cd $CUR
done
