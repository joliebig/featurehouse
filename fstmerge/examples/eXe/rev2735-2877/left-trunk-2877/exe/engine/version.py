"""
Version Information
"""
import re
project        = "exe"
release        = "0.24.1"
revision       = "$Revision: 2859 $"[11:-2]
try:
    from version_svn import revision
except ImportError:
    pass
revision = revision.replace(':', '-')
version        = release + "." + revision
mo = re.match('(\d+)\D+', revision)
if mo:
    build = release + '.' + mo.group(1)
else:
    build = version
if __name__ == '__main__':
    print project, version, build
