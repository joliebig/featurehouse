from exe.engine.path import Path, TempDirPath
import os, sys
def main():
    if len(sys.argv) < 2:
        print 'Usage: %s [version]' % sys.argv[0]
        print 'Where [version] is the branch you want to checkout'
        print 'Eg. %s 0.7' % sys.argv[0]
    else:
        version = sys.argv[1]
        branch = 'http://exe.cfdl.auckland.ac.nz/svn/exe/branches/%s' % version
        origDir = Path(sys.argv[0]).abspath().dirname()
        tmp = TempDirPath()
        os.chdir(tmp)
        os.system('svn export %s exe' % branch)
        (origDir/'../../exe/webui/firefox').copytree(tmp/'exe/exe/webui/firefox')
        os.chdir(tmp/'exe')
        tarball = Path('../exe-%s-source.tgz' % version).abspath()
        os.system('tar czf %s *' % tarball)
        os.chdir(tmp)
        if '--local' not in sys.argv:
            open('sftpbatch.tmp', 'w').write(
                'cd /home/pub/exe\n'
                'put %s\n' % tarball)
            os.system('sftp -b sftpbatch.tmp matiu@shell.eduforge.org')
        if os.getuid() == 0:
            tarball.copyfile('/usr/portage/distfiles/' + tarball.basename())
        os.chdir(tmp/'exe/installs/gentoo')
        newEbuildFilename = Path('exe-%s.ebuild' % version).abspath()
        if not newEbuildFilename.exists():
            Path('exe-0.7.ebuild').copy(newEbuildFilename)
        if os.getuid() == 0:
            ebuildDir = Path('/usr/local/portage/dev-python/exe')
            if ebuildDir.exists():
                ebuildDir.rmtree()
            ebuildDir.makedirs()
            os.chdir(ebuildDir)
            newEbuildFilename.copy(ebuildDir)
            filesDir = ebuildDir/'files'
            filesDir.makedirs()
            Path(tmp/'exe/installs/gentoo/all-config.patch').copy(filesDir)
            if '--local' not in sys.argv:
                oldTarball = Path('/usr/portage/distfiles/')/tarball.basename()
                if oldTarball.exists():
                    oldTarball.remove()
                os.environ['GENTOO_MIRRORS']=''
                os.system('ebuild %s fetch' % newEbuildFilename.basename())
            os.system('ebuild %s manifest' % newEbuildFilename.basename())
            os.system('ebuild %s digest' % newEbuildFilename.basename())
            if '--install' in sys.argv:
                os.system('ebuild %s install' % newEbuildFilename.basename())
if __name__ == '__main__':
    main()
