"""HistToGNU.py
    Convert saved binary pickle of histograms to gnu plot output
Usage: %(program)s [options] [histogrampicklefile ...]
reads pickle filename from options if not specified
writes to stdout
"""
globalOptions = """
set grid
set xtics 5
set xrange [0.0:100.0]
"""
dataSetOptions = "smooth unique"
import sys
from spambayes.Options import options
from spambayes.safepickle import pickle_read, pickle_write
program = sys.argv[0]
def usage(code, msg=''):
    """Print usage message and sys.exit(code)."""
    if msg:
        print(msg, file=sys.stderr)
        print(file=sys.stderr)
    print(__doc__ % globals(), file=sys.stderr)
    sys.exit(code)
def loadHist(path):
    """Load the histogram pickle object"""
    return pickle_read(path)
def outputHist(hist, f=sys.stdout):
    """Output the Hist object to file f"""
    hist.fill_buckets()
    for i in range(len(hist.buckets)):
        n = hist.buckets[i]
        f.write("%.3f %d\n" % ( (100.0 * i) / hist.nbuckets, n))
def plot(files):
    """given a list of files, create gnu-plot file"""
    import io
    cmd = io.StringIO()
    cmd.write(globalOptions)
    args = []
    for file in files:
        args.append("""'-' %s title "%s" """ % (dataSetOptions, file))
    cmd.write('plot %s\n' % ",".join(args))
    for file in files:
        outputHist(loadHist(file), cmd)
        cmd.write('e\n')
    cmd.write('pause 100\n')
    print(cmd.getvalue())
def main():
    import getopt
    try:
        opts, args = getopt.getopt(sys.argv[1:], '', [])
    except getopt.error as msg:
        usage(1, msg)
    if not args and options["TestDriver", "save_histogram_pickles"]:
        args = []
        for f in ('ham', 'spam'):
            fname = "%s_%shist.pik" % (options["TestDriver",
                                               "pickle_basename"], f)
            args.append(fname)
    if args:
        plot(args)
    else:
        print("could not locate any files to plot")
if __name__ == "__main__":
    main()
