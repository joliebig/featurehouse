#!/bin/bash

export PATH=$PATH:`pwd`
EXAMPLEFOLDER=$1
EVALUATIONFOLDER="../evaluation/"`basename $EXAMPLEFOLDER`
CURRENTFOLDER=`pwd`
LANGUAGE="java"
PROCESSMERGERESULTS=process_merge_results.sh

#Write results for mergetripel
cd $EXAMPLEFOLDER
for expressionfile in `find . -maxdepth 2 -mindepth 1 -name "*.revisions"`
	do
		date
		echo $expressionfile
		MERGEDIR=`dirname $expressionfile`/`basename $expressionfile .revisions`
		echo $MERGEDIR
		$PROCESSMERGERESULTS $MERGEDIR $LANGUAGE merge
		date
done;

#Write complete results
if [ -f result ]
then
  rm result
fi

for i in `find . -maxdepth 2 -mindepth 1 -name "result" | sort`;
do
	echo $i >> result
	cat $i >> result
	echo >> result
done

#Convert result to CSV
java -jar $CURRENTFOLDER/resultConvertCSV.jar result

if [ ! -d $EVALUATIONFOLDER ]
then
	mkdir -p $EVALUATIONFOLDER
fi
mv *.csv $EVALUATIONFOLDER
cd $CURRENTFOLDER
