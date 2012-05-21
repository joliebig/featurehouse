#!/bin/bash

if [[ $# -ne "3" ]]
 then
  echo "This scripts counts the number of conflicting lines in a semistructed merge and an unstructured merge."
  echo "MERGEINFODIR: directory with merge information"
  echo ""
  echo "Usage `basename $0` MERGEINFODIR SEMISTRUCTUREDEXTENSION UNSTRUCTUREDEXTENSION"
  echo "Example `basename $0` rev100-150 java merge"
fi

MINFODIR=$1
REPORTDIR=`dirname $MINFODIR`
LANGUAGE=$2
MERGE=$3

RESULTFILESTOTAL="result-files-total-sorted"
RESULTJAVA="result-java"
RESULTMERGE="result-merge"
RESULTTOTAL="result"

CCLSCRIPTFILES="count_conflicting_lines-2.py"
SORT="sort"

echo "Calcualting and sorting conflicts"
$CCLSCRIPTFILES --dir=$MINFODIR --fext=.$LANGUAGE,.$MERGE | $SORT +2 -3 > $REPORTDIR/$RESULTFILESTOTAL

echo "Writting $LANGUAGE conflicts"
$CCLSCRIPTFILES --dir=$MINFODIR --fext=.$LANGUAGE > $REPORTDIR/$RESULTJAVA

echo "Writing MERGE conflicts"
$CCLSCRIPTFILES --dir=$MINFODIR --fext=.$MERGE > $REPORTDIR/$RESULTMERGE

CCLSCRIPT="count_conflicting_lines.py"
LOCSCRIPT="loc.sh"

echo "Semistructured merge" > $REPORTDIR/$RESULTTOTAL
$CCLSCRIPT --dir=$MINFODIR --fext=.$LANGUAGE >> $REPORTDIR/$RESULTTOTAL && $LOCSCRIPT $MINFODIR $LANGUAGE >> $REPORTDIR/$RESULTTOTAL

echo "Unstructured merge"  >> $REPORTDIR/$RESULTTOTAL
$CCLSCRIPT --dir=$MINFODIR --fext=.$MERGE  >> $REPORTDIR/$RESULTTOTAL && $LOCSCRIPT $MINFODIR $MERGE  >> $REPORTDIR/$RESULTTOTAL

cat $REPORTDIR/$RESULTTOTAL
