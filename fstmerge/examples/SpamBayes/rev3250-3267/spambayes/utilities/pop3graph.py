"""Analyse the pop3proxy's caches and produce a graph of how accurate
classifier has been over time.  Only really meaningful if you started
with an empty database."""

import sys

import getopt

from spambayes import  mboxutils

from spambayes.FileCorpus import FileCorpus, FileMessageFactory, GzipFileMessageFactory

from spambayes.Options import options

def usage():

    print(__doc__)
 def main(argv):

    opts, args = getopt.getopt(argv, "h", ["help"])

    for opt, arg in opts:

        if opt in ("-h", "--help"):

            usage()

            return

    if options["pop3proxy", "cache_use_gzip"]:

        messageFactory = GzipFileMessageFactory()

    else:

        messageFactory = FileMessageFactory()

    sc = get_pathname_option("Storage", "spam_cache")

    hc = get_pathname_option("Storage", "ham_cache")

    spamCorpus = FileCorpus(messageFactory, sc)

    hamCorpus = FileCorpus(messageFactory, hc)

    allTrained = {}

    for corpus, disposition in [(spamCorpus, 'Yes'), (hamCorpus, 'No')]:

        for m in corpus:

            message = mboxutils.get_message(m.getSubstance())

            message._pop3CacheDisposition = disposition

            allTrained[m.key()] = message

    keys = list(allTrained.keys())

    keys.sort()

    limit = 70

    if len(keys) < limit:

        scale = 1

    else:

        scale = len(keys) // (limit//2)

    count = successful = 0

    successByCount = []

    for key in keys:

        message = allTrained[key]

        disposition = message[options["Headers",
                                      "classification_header_name"]]

        if (message._pop3CacheDisposition == disposition):

            successful += 1

        count += 1

        if count % scale == (scale-1):

            successByCount.append(successful // scale)

    size = count // scale

    graph = [[" " for i in range(size+3)] for j in range(size)]

    for c in range(size):

        graph[c][1] = "|"

        graph[c][c+3] = "."

        graph[successByCount[c]][c+3] = "*"

    graph.reverse()

    print("\n   Success of the classifier over time:\n")

    print("   . - Number of messages over time")

    print("   * - Number of correctly classified messages over time\n\n")

    for row in range(size):

        line = ''.join(graph[row])

        if row == 0:

            print(line + " %d" % count)

        elif row == (count - successful) // scale:

            print(line + " %d" % successful)

        else:

            print(line)

    print(" " + "_" * (size+2))
 if __name__ == '__main__':

    main(sys.argv[1:])

 if __name__ == '__main__':

    main(sys.argv[1:])





