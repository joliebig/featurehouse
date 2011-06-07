#!/usr/bin/python
# -*- coding: utf-8 -*-

import os
import re
import sys
from optparse import OptionParser, OptionGroup

def returnFileNames(folder, extfilt = ['.java']):
	'''This function returns all files of the input folder <folder>
	and its subfolders.'''
	filesfound = list()

	if os.path.isdir(folder):
		wqueue = [os.path.abspath(folder)]

		while wqueue:
			currentfolder = wqueue[0]
			wqueue = wqueue[1:]
			foldercontent = os.listdir(currentfolder)
			tmpfiles = filter(lambda n: os.path.isfile(
					os.path.join(currentfolder, n)), foldercontent)
			tmpfiles = filter(lambda n: os.path.splitext(n)[1] in extfilt,
					tmpfiles)
			tmpfiles = map(lambda n: os.path.join(currentfolder, n),
					tmpfiles)
			filesfound += tmpfiles
			tmpfolders = filter(lambda n: os.path.isdir(
					os.path.join(currentfolder, n)), foldercontent)
			tmpfolders = map(lambda n: os.path.join(currentfolder, n),
					tmpfolders)
			wqueue += tmpfolders

	return filesfound


class CCLines:

	# regular expressions for the conflict indicators
	revar1conf = re.compile('^(.*)<<<<<<<.*')
	reconfbar  = re.compile('^(.*)=======.*')
	revar2conf = re.compile('^(.*)>>>>>>>.*')
	# ~ is the conflict tag for semantic conflicts
	resemconf  = re.compile('(~~FSTMerge~~)')

	def __init__(self):
		# define and check program options
		oparser = OptionParser()
		oparser.add_option("-f", "--file", dest="file", help="input file (mandatory or dir)")
		oparser.add_option("-d", "--dir", dest="dir", help="input dir (mandatory or file)")
		oparser.add_option("-e", "--fext", dest="fext", help="file extension filter, when used with --dir, e.g. --fext=.java or --fext=.java,.c")
		group = OptionGroup(oparser, "Result",
				"The result of the program is a six tuple. The first element "
				"is the number of syntactic conflicts. The second and the third are "
				"the number of conflicting lines of variant 1 and 2 respectively, "
				"which are involved in conflicts. The fourth element is "
				"the number of semantic conflicts. The fifth and sixth are the number"
				"of files and of files that are involved in conflicts.")
		oparser.add_option_group(group)
		(opts, args) = oparser.parse_args()

		if (not opts.file and not opts.dir):
			oparser.print_help()
			sys.exit(-1)
		if (opts.dir and not opts.fext):
			oparser.print_help()
			sys.exit(-1)

		self.count(opts)

	def __handleDir(self, dir, fext):
		fext = fext.split(',')
		files = returnFileNames(dir, fext)
		synconflicts = 0
		semconflicts = 0
		synvar1conflictinglines = 0
		synvar2conflictinglines = 0
		numFiles = 0
		numConflictFiles = 0

		for file in files:
			(s1, s2, s3, s4) = self.__countLinesOfFile(file)
			synconflicts += s1
			synvar1conflictinglines += s2
			synvar2conflictinglines += s3
			semconflicts += s4
			numFiles += 1
			if s1 > 0:
				numConflictFiles += 1

		return (synconflicts, synvar1conflictinglines,
				synvar2conflictinglines, semconflicts, numFiles, numConflictFiles)

	def __countLinesOfFile(self, file):
		# result values
		synconflicts = 0
		semconflicts = 0
		synvar1conflictinglines = 0
		synvar2conflictinglines = 0

		# status variables for counting conflicts
		synvar1conflict = False
		synvar2conflict = False
		middle = False
		file = os.path.abspath(file)

		if not os.path.exists(file):
			print('ERROR: file '+file+' not found!')
		else:
			fd = open(os.path.abspath(file), 'r')
			for line in fd:
				if line.strip() == "":
					continue
				if CCLines.resemconf.search(line):
					semconflicts += len(CCLines.resemconf.findall(line))

					continue
				if CCLines.revar1conf.findall(line):
					synvar1conflict = True
					synconflicts += 1

					continue
				if CCLines.reconfbar.findall(line) and synvar1conflict:
					# add additional element in case code is found in this line
					# e.g., '   }========= /home/joliebig/tmp/Var2.java'
					res = CCLines.reconfbar.search(line)
					s = res.groups()[0]
					s.strip()

					if s != '':
						synvar1conflictinglines += 1

					middle = True
					synvar1conflict = False
					synvar2conflict = True
					continue
				if CCLines.revar2conf.findall(line) and middle:
					# add additional element in case code is found in this line
					# e.g., '   }>>>>>>>> /home/joliebig/tmp/Var2.java'
					res = CCLines.revar2conf.search(line)
					s = res.groups()[0]
					s.strip()

				
					if s != '':
						synvar2conflictinglines += 1

					middle = False
					synvar2conflict = False
					continue
				if synvar1conflict:
					synvar1conflictinglines += 1
					continue
				if synvar2conflict:
					synvar2conflictinglines += 1
					continue
		if synconflicts > 0 or synvar1conflictinglines > 0 or synvar2conflictinglines > 0 or semconflicts > 0 :
		 print('INFO: processing %s with (%s, %s, %s, %s)' % (os.path.relpath(file), synconflicts, synvar1conflictinglines, synvar2conflictinglines, semconflicts))
		return (synconflicts, synvar1conflictinglines,
				synvar2conflictinglines, semconflicts)

	def count(self, opts):
		if opts.file:
			# print(self.__countLinesOfFile(opts.file))
			self.__countLinesOfFile(opts.file)
		else:
			# print(self.__handleDir(opts.dir, opts.fext))
			self.__handleDir(opts.dir, opts.fext)

##################################################
if __name__ == '__main__':
	cs = CCLines()
