#!/bin/bash

PROJECTS=`find -maxdepth 1 -type d`
for PRJ in $PROJECTS; do
	if [[ $PRJ != . ]]
	then
		echo "########################"
		echo "# " `basename $PRJ`
		echo "########################"
		FEATURES=`find $PRJ -maxdepth 1 -type d`
		for FEATURE in $FEATURES; do
			if [[ $FEATURE != $PRJ ]]
			then
				echo `basename $FEATURE` `find $FEATURE -name "*.java" | xargs grep -v ^$ | wc -l`
			fi
		done
	fi
done