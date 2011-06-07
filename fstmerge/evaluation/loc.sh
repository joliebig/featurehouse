#!/bin/bash

if [[ $# -ne "2" ]] 
  then
   echo "Counts LOC in directory"
   echo "" 
   echo "Does not strip comments"
   echo "Usage: `basename $0` DIRECTORY EXTENSION"
   echo "Example: `basename $0` dir/ java"
   exit 1;
fi

echo -n "LOC with comments: "
find $1 -name "*.$2" | xargs cat | wc -l

echo -n "LOC without comments: "
find $1 -name "*.$2" | xargs sed '/^\s*$/d' | wc -l

