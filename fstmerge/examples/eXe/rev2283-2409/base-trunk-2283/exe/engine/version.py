"""
Version Information
2006-04-03
"""
project        = "exe"
release        = "0.18"
revision       = "$Revision: 2173 $"[11:-2]
version        = release + "." + revision
if __name__ == '__main__':
    print project, version
