#!/bin/bash

PRJ=`find -type f -name "*Comp.features"`
SUFFIX=Comp
TARGET1=RSF
TARGET2=XML

rm -rf $TARGET1
rm -rf $TARGET2
	
for I in $PRJ; do
	DIR=`dirname $I`
	rm -rf $DIR/`basename $I .features`
done
