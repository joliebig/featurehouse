#!/bin/bash

PRJ=`find -type f -name "*Comp.features"`
SUFFIX2=.rsf
TARGET=RSF
PWD=`pwd`

mkdir $TARGET
	
for I in $PRJ; do
	java -jar ../../lib/FeatureHouse.jar --count --expression $I
	mkdir -p $TARGET/`dirname $I$SUFFIX2`
	mv -f $I$SUFFIX2 $TARGET/`dirname $I$SUFFIX2`/
done
