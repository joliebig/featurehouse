"""
Version Information
2006-04-03
"""
project        = "exe"
release        = "0.19"
revision       = "$Revision: 2321 $"[11:-2]
version        = release + "." + revision
if __name__ == '__main__':
    print project, version
