#! /usr/bin/env bash
 
#echo $1
#echo $2
#echo $3

mergeDir="merge/"

for f in `find $1 -name "*.java"`
do
	#strip first directory
	stripDir=${f/$1}

	#create mergedir
	mkdir -p `dirname $mergeDir$stripDir`
	
	#apply merge	
	merge -p -q $f $2$stripDir $3$stripDir > $mergeDir$stripDir
	#echo $f $2$stripDir $3$stripDir $mergeDir$stripDir	
done
