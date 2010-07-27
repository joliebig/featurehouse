import os

import sys

from setuptools import setup, find_packages

if sys.version_info < (2, 4):

    print("Error: You need at least Python 2.4 to use SpamBayes.")

    print("You're running version %s." % sys.version)

    sys.exit(0)



from distutils.core import setup

from spambayes import __version__

import distutils.command.install_scripts

parent = distutils.command.install_scripts.install_scripts

class  install_scripts (parent) :
	old_scripts=[
        'unheader',
        'hammie',
        'hammiecli',
        'hammiesrv',
        'hammiefilter',
        'pop3proxy',
        'smtpproxy',
        'sb_smtpproxy',
        'proxytee',
        'dbExpImp',
        'mboxtrain',
        'imapfilter',
        'notesfilter',
        ]
	    def run(self):

        err = False

        for s in self.old_scripts:

            s = os.path.join(self.install_dir, s)

            for e in (".py", ".pyc", ".pyo"):

                if os.path.exists(s+e):

                    print("Error: old script", s+e, end=' ', file=sys.stderr)

                    print("still exists.", file=sys.stderr)

                    err = True

        if err:

            print("Do you want to delete these scripts? (y/n)", file=sys.stderr)

            answer = input("")

            if answer == "y":

                for s in self.old_scripts:

                    s = os.path.join(self.install_dir, s)

                    for e in (".py", ".pyc", ".pyo"):

                        try:

                            os.remove(s+e)

                            print("Removed", s+e)

                        except OSError:

                            pass

        return parent.run(self)

import distutils.command.sdist sdist_parent = distutils.command.sdist.sdist class  sdist (sdist_parent) :
	"""Like the standard sdist, but also prints out MD5 checksums and sizes
    for the created files, for convenience."""
	    def run(self):

        import md5

        retval = sdist_parent.run(self)

        for archive in self.get_archive_files():

            data = file(archive, "rb").read()

            print('\n', archive, "\n\tMD5:", md5.md5(data).hexdigest())

            print("\tLength:", len(data))

        return retval

scripts=['scripts/sb_client.py',
         'scripts/sb_dbexpimp.py',
         'scripts/sb_evoscore.py',
         'scripts/sb_filter.py',
         'scripts/sb_bnfilter.py',
         'scripts/sb_bnserver.py',
         'scripts/sb_imapfilter.py',
         'scripts/sb_mailsort.py',
         'scripts/sb_mboxtrain.py',
         'scripts/sb_notesfilter.py',
         'scripts/sb_pop3dnd.py',
         'scripts/sb_server.py',
         'scripts/core_server.py',
         'scripts/sb_unheader.py',
         'scripts/sb_upload.py',
         'scripts/sb_xmlrpcserver.py',
         'scripts/sb_chkopts.py',
        ] if sys.platform == 'win32':

    scripts.append('windows/pop3proxy_service.py')

    scripts.append('windows/pop3proxy_tray.py')

 if sys.version_info >= (3, 0):

    lf_min_version = "0.6"

else:

    lf_min_version = "0.2"

 if sys.platform == 'win32':

    scripts.append('windows/pop3proxy_service.py')

    scripts.append('windows/pop3proxy_tray.py')

 if sys.version_info >= (3, 0):

    lf_min_version = "0.6"

else:

    lf_min_version = "0.2"



setup(
    name='spambayes',
    version = __version__,
    description = "Spam classification system",
    author = "the spambayes project",
    author_email = "spambayes@python.org",
    url = "http://spambayes.sourceforge.net",
    install_requires = ["lockfile>=%s" % lf_min_version,
                        "pydns>=2.0"],
    cmdclass = {'install_scripts': install_scripts,
                'sdist': sdist,
                },
    scripts=scripts,
    packages = [
        'spambayes',
        'spambayes.resources',
        'spambayes.core_resources',
        ],
    classifiers = [
        'Development Status :: 5 - Production/Stable',
        'Environment :: Console',
        'Environment :: Plugins',
        'Environment :: Win32 (MS Windows)',
        'License :: OSI Approved :: Python Software Foundation License',
        'Operating System :: POSIX',
        'Operating System :: MacOS :: MacOS X',
        'Operating System :: Microsoft :: Windows :: Windows 95/98/2000',
        'Operating System :: Microsoft :: Windows :: Windows NT/2000',
        'Natural Language :: English',
        'Programming Language :: Python',
        'Programming Language :: C',
        'Intended Audience :: End Users/Desktop',
        'Topic :: Communications :: Email :: Filters',
        'Topic :: Communications :: Email :: Post-Office :: POP3',
        'Topic :: Communications :: Email :: Post-Office :: IMAP',
        ],
    )

if sys.version_info < (2, 4):

    print "Error: You need at least Python 2.4 to use SpamBayes."

    print "You're running version %s." % sys.version

    sys.exit(0)



