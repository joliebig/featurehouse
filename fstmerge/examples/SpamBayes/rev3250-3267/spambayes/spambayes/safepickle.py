"""Lock pickle files for reading and writing."""

import sys

import os

import pickle as pickle

import lockfile

from spambayes.Options import options

def pickle_read(filename):

    """Read pickle file contents with a lock."""

    lock = lockfile.FileLock(filename)

    lock.acquire(timeout=20)

    try:

        return pickle.load(open(filename, 'rb'))

    finally:

        lock.release()
 def pickle_write(filename, value, protocol=0):

    '''Store value as a pickle without creating corruption'''

    lock = lockfile.FileLock(filename)

    lock.acquire(timeout=20)

    try:

        tmp = filename + '.tmp'

        fp = None

        try: 

            fp = open(tmp, 'wb') 

            pickle.dump(value, fp, protocol) 

            fp.close() 

        except IOError as e: 

            if options["globals", "verbose"]: 

                print('Failed update: ' + str(e), file=sys.stderr)

            if fp is not None: 

                os.remove(tmp) 

            raise

        try:

            os.rename(tmp, filename)

        except OSError:

            os.rename(filename, filename + '.bak')

            os.rename(tmp, filename)

            os.remove(filename + '.bak')

    finally:

        lock.release()




