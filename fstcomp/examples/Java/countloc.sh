#!/bin/bash

PRJ=`find -type d -name "*Comp"`
for I in $PRJ; do
	echo "find $I -name "*.java" | xargs grep -v ^$ | wc -l"
	find $I -name "*.java" | xargs grep -v ^$ | wc -l
done