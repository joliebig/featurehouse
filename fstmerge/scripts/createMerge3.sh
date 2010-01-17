#! /usr/bin/env bash

# Global variables
LEFTDIR=$1
BASEDIR=$2
RIGHTDIR=$3
MERGEDIR="merge/"

EMPTYFILE="MergedWithEmptyFile"

# trap keyboard interrupt ctrl-c
trap quit SIGINT;

quit() {
	echo "Received SIGINT. Quitting."
	tidyUp
	exit 2;
}

checkParameters() {
	# check mininal number of paramters	
	if [[ $# -lt "3" ]]
		then
			showHelpMsg;
			exit 1;
	fi
	# check if given directories exist
	if [[ -d $1 && -d $2 && -d $3 ]]
		then
			# directories must end with a /
			echo "${LEFTDIR}" | grep ".*/" > /dev/null 2>&1
			if [[ "${?}" -ne "0" ]]
				then
					LEFTDIR=$LEFTDIR/
			fi;
			echo "${BASEDIR}" | grep ".*/" > /dev/null 2>&1
			if [[ "${?}" -ne "0" ]]
				then
					BASEDIR=$BASEDIR/
			fi;
			echo "${RIGHTDIR}" | grep ".*/" > /dev/null 2>&1
			if [[ "${?}" -ne "0" ]]
				then
					RIGHTDIR=$RIGHTDIR/
			fi;
		else
			echo "One or more direcotries don't exsist.";
			exit 1;
	fi
	# check for set MERGEDIR
	if [[ $# -eq "4" ]]
		then
			MERGEDIR=$4;
			# create directory is not exsits
			mkdir -p $MERGEDIR;
			# directories must end with a /
			echo "${MERGEDIR}" | grep ".*/" > /dev/null 2>&1
			if [[ "${?}" -ne "0" ]]
				then
					MERGEDIR=$MERGEDIR/
			fi;
	fi
	echo "Merging: $LEFTDIR, $BASEDIR, $RIGHTDIR to $MERGEDIR";
}

showHelpMsg() {
	echo "`basename $0` applies GNU merge recursivly on three given directories."
	echo 
	echo "Usage: `basename $0` left-directory/ base-directory/ right-directory/ merge-output-directory/"
	echo 
	echo "The parameter merge-output-directory is optional. If it is omitted output will be written to $MERGEDIR."
	echo 
}

# apply merge on file present in left and/or base and right
iterateLeft () {
	# iterate over files in $LEFTDIR
	for f in `find $LEFTDIR -name "*.java"`
		do
			#strip first directory
			stripDir=${f/$LEFTDIR}
			
			LEFTFILE=$f;	
			BASEFILE=$BASEDIR$stripDir;
			RIGHTFILE=$RIGHTDIR$stripDir;
			MERGEFILE=$MERGEDIR$stripDir;
			
			# echo "Trying to merge: $LEFTFILE $BASEFILE $RIGHTFILE to $MERGEFILE"

			# test if file exists in all three directories
			if [[ -e "$BASEFILE" && -e "$RIGHTFILE" ]]
				then
					# echo "Normal merge: $LEFTFILE $BASEFILE $RIGHTFILE to $MERGEFILE"
					#create mergedir
					mkdir -p `dirname $MERGEFILE`;
					#merge files
					mergeFiles $LEFTFILE $BASEFILE $RIGHTFILE $MERGEFILE;
					continue;	
			fi
					

			# test if file exists in one other directory			
			if [[ -e "$BASEFILE" || -e "$RIGHTFILE" ]]
				then					
					if [[ -e "$BASEFILE" ]]
						# if basefile exists create empty RIGHTFILE
						then
							echo "Missing right file: $RIGHTFILE"
							RIGHTFILE=$EMPTYFILE;
						# else create empty BASEFILE
						else
							echo "Missing base file: $BASEFILE"
							BASEFILE=$EMPTYFILE;
					fi
					#create mergedir
					mkdir -p `dirname $MERGEFILE`;
					#merge files
					echo " Empty file merge: $LEFTFILE $BASEFILE $RIGHTFILE to $MERGEFILE"
					mergeFiles $LEFTFILE $BASEFILE $RIGHTFILE $MERGEFILE;
					continue;					
			fi			
			
			echo "Can't merge $LEFTFILE. No merge partners found."

		done
}

# apply merge on files present in right and base but not in left
iterateRight() {
	for f in `find $RIGHTDIR -name "*.java"`
		do
			#strip first directory
			stripDir=${f/$RIGHTDIR}
			
			RIGHTFILE=$f;	
			BASEFILE=$BASEDIR$stripDir;
			LEFTFILE=$LEFTDIR$stripDir;
			MERGEFILE=$MERGEDIR$stripDir;
			
			# if file is in left then file is already merged
			if [[ -e "$LEFTFILE" ]]
				then
					continue;
			fi
			
			# if file is in base, create empty left file and merge
			if [[ -e "$BASEFILE" ]]
				then
					echo "Missing left file: $LEFTFILE"
					LEFTFILE=$EMPTYFILE;
					mkdir -p `dirname $MERGEFILE`;
					#merge files
					echo " Empty file merge: $LEFTFILE $BASEFILE $RIGHTFILE to $MERGEFILE"
					mergeFiles $LEFTFILE $BASEFILE $RIGHTFILE $MERGEFILE;
			fi
			
		done
}

mergeFiles() {
	# applyMerge
	merge -p -q $1 $2 $3 > $4
}

createEmptyFile() {
	touch $EMPTYFILE;
}

tidyUp() {
	rm $EMTPYFILE > /dev/null 2>&1
}

# main()
checkParameters $*

createEmptyFile

iterateLeft
iterateRight

tidyUp

echo "Done."


