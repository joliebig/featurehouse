#!/bin/bash

PROJECTS=`find -maxdepth 1 -type d`
for PRJ in $PROJECTS; do
	if [[ $PRJ != . ]]
	then
		FEATURES=`find $PRJ -maxdepth 1 -type d`
		for FEATURE in $FEATURES; do
			if [[ $FEATURE != $PRJ ]]
			then
				if [[ `basename $FEATURE` != CVS ]]
				then			
					echo `basename $PRJ` `basename $FEATURE` `find $FEATURE -name "*.java" | xargs grep -v ^$ | wc -l`
				fi
			fi
		done
	fi
done