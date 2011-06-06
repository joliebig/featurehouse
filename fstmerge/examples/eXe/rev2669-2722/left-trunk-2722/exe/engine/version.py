"""
Version Information
2006-04-03
"""
project        = "exe"
release        = "0.22.1"
revision       = "$Revision: 2682 $"[11:-2]
try:
    from version_svn import revision
except ImportError:
    pass
version        = release + "." + revision
if __name__ == '__main__':
    print project, version
