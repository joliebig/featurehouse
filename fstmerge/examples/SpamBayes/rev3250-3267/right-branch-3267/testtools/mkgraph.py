"""
This takes incremental.py output and outputs a file to be
used to create a graph (by default by plotmtv).
Options:
  -h                 Display this message.
  -r [report type]   Output this type of report.
                     Currently supported: "error", "counts"
                     Defaults to "error".
  -s [number]        Span of days to average counts over.
                     If not specified then culmulative counts are
                     output (this is the default).
  -f [file]          Input file (if not specified, stdin is used)
  -c                 Rather than outputting in plotmtv format,
                     where each line is described separately,
                     output each line in a separate column, which
                     is easier to create an Excel graph from.
  -s [sep]           If -c is used, then this is the column
                     separator (defaults to comma).
"""
import sys
import getopt
report = "error"
span = None
set = ""
nham_tested = []
nham_trained = []
nham_right = []
nham_wrong = []
nham_unsure = []
nspam_tested = []
nspam_trained = []
nspam_right = []
nspam_wrong = []
nspam_unsure = []
def outputset(Output):
    global report
    global span
    global set
    global nham_tested
    global nham_trained
    global nham_right
    global nham_wrong
    global nham_unsure
    global nspam_tested
    global nspam_trained
    global nspam_right
    global nspam_wrong
    global nspam_unsure
    if set == "":
        return
    if span:
        title = "%d-Day Average" % span
    else:
        title = "Cumulative"
    if report == "counts":
        Output.output_title(title)
        color = 0
        for data, label in [(nham_tested, "ham_tested"),
                            (nham_trained, "ham_trained"),
                            (nham_right, "ham_right"),
                            (nham_wrong, "ham_wrong"),
                            (nham_unsure, "ham_unsure"),
                            (nspam_tested, "spam_tested"),
                            (nspam_trained, "spam_trained"),
                            (nspam_right, "spam_right"),
                            (nspam_wrong, "spam_wrong"),
                            (nspam_unsure, "spam_unsure"),
                            ]:
            Output.add_line(data, linelabel=label, linecolor=color)
            color += 1
        Output.output()
    if report == "error":
        Output.output_title(title)
        Output.line_title(linelabel="fp", linecolor=0)
        for k in range(len(nham_wrong)):
            n = nham_wrong[k]
            d = nham_tested[k]
            if span and k - span >= 0:
                n -= nham_wrong[k - span]
                d -= nham_tested[k - span]
            Output.add_line(k, (n * 100.0 / (d or 1)))
        Output.line_title(linelabel="fn", linecolor=1)
        for k in range(len(nspam_wrong)):
            n = nspam_wrong[k]
            d = nspam_tested[k]
            if span and k - span >= 0:
                n -= nspam_wrong[k - span]
                d -= nspam_tested[k - span]
            Output.add_line(k, (n * 100.0 / (d or 1)))
        Output.line_title(linelabel="unsure", linecolor=2)
        for k in range(len(nspam_unsure)):
            n = nham_unsure[k] + nspam_unsure[k]
            d = nham_tested[k] + nspam_tested[k]
            if span and k - span >= 0:
                n -= nham_unsure[k - span] + nspam_unsure[k - span]
                d -= nham_tested[k - span] + nspam_tested[k - span]
            Output.add_line(k, (n * 100.0 / (d or 1)))
        Output.line_title(linelabel="training_is_ham", linecolor=3)
        for k in range(len(nspam_unsure)):
            n = nham_trained[k]
            d = nham_trained[k] + nspam_trained[k]
            if span and k - span >= 0:
                n -= nham_trained[k - span]
                d -= nham_trained[k - span] + nspam_trained[k - span]
            Output.add_line(k, (n * 100.0 / (d or 1)))
        Output.output()
    set = ""
    nham_tested = []
    nham_trained = []
    nham_right = []
    nham_wrong = []
    nham_unsure = []
    nspam_tested = []
    nspam_trained = []
    nspam_right = []
    nspam_wrong = []
    nspam_unsure = []
class SetOutputter(object):
    """Class to output set data in the correct format."""
    def __init__(self, sep=',', immediate_print=False):
        self.sep = sep
        self.immediate_print = immediate_print
        self.reset()
    def output_title(self, title):
        if self.immediate_print:
            title = '$ Data=Curve2d name="%s Counts"' % (title)
        print(title)
        if not self.immediate_print:
            print(self.sep.join(["group", "ham_tested", "ham_trained",
                                 "ham_right", "ham_wrong", "ham_unsure",
                                 "spam_tested", "spam_trained",
                                 "spam_right", "spam_wrong",
                                 "spam_unsure"]))
    def add_line(self, vals, linetype=1, linelabel="", markertype=0,
                 linecolor=0):
        if self.immediate_print:
            print()
            print('%% linetype=%d linelabel="%s" markertype=%d linecolor=%s' % \
                  (linetype, linelabel, markertype, linecolor))
        for k in range(len(vals)):
            n = vals[k]
            if span and k - span >= 0:
                n -= vals[k - span]
            if k in self.lines:
                self.lines[k].append(str(n))
            else:
                self.lines[k] = [str(n)]
            if self.immediate_print:
                print('%d %d' % (k, n))
    def output(self):
        if not self.immediate_print:
            keys = list(self.lines.keys())
            keys.sort()
            for k in keys:
                vals = [str(k)]
                vals.extend(self.lines.get(k, []))
                print(self.sep.join(vals))
        else:
            print()
        self.reset()
    def reset(self):
        self.lines = {}
class ErrorSetOutputter(SetOutputter):
    """Class to output set error data in the correct format."""
    def output_title(self, title):
        if self.immediate_print:
            print('$ Data=Curve2d')
            print('%% toplabel="%s Error Rates"' % (title))
            print('% ymax=5')
            print('% xlabel="Days"')
            print('% ylabel="Percent"')
        else:
            print(title)
            print(self.sep.join(["group", "fp", "fn", "unsure",
                                 "training_is_ham"]))
    def line_title(self, linetype=1, linelabel="", markertype=0,
                   linecolor=0):
        if self.immediate_print:
            print('\n%% linetype=%d linelabel="%s" markertype=%d ' \
                  'linecolor=%d' % (linetype, linelabel, markertype,
                                    linecolor))
    def add_line(self, k, v):
        if self.immediate_print:
            print('%d %f' % (k, v))
        else:
            if k in self.lines:
                self.lines[k].append(str(v))
            else:
                self.lines[k] = [str(v)]
def main():
    global report
    global span
    global set
    global nham_tested
    global nham_trained
    global nham_right
    global nham_wrong
    global nham_unsure
    global nspam_tested
    global nspam_trained
    global nspam_right
    global nspam_wrong
    global nspam_unsure
    filename = None
    sep = ','
    all_together = False
    opts, args = getopt.getopt(sys.argv[1:], 's:r:f:hcs:')
    for opt, arg in opts:
        if opt == '-s':
            span = int(arg)
        elif opt == '-r':
            report = arg
        elif opt == '-f':
            filename = arg
        elif opt == '-c':
            all_together = True
        elif opt == '-s':
            sep = arg
        elif opt == '-h':
            print(__doc__)
            sys.exit()
    if report not in ("error", "counts"):
        print("Unrecognized report type", file=sys.stderr)
        sys.exit(1)
    if report == "counts":
        Output = SetOutputter(sep, not all_together)
    elif report == "error":
        Output = ErrorSetOutputter(sep, not all_together)
    if filename:
        source = file(filename)
    else:
        source = sys.stdin
    while 1:
        line = source.readline()
        if line == "":
            break
        if line.endswith("\n"):
            line = line[:-1]
        if line.startswith("Set "):
            outputset(Output)
            set = line[4:]
        if len(line) > 0 and (line[0] in ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')):
            vals = line.split(" ")
            nham_tested.append(int(vals[0]))
            nham_trained.append(int(vals[1]))
            nham_right.append(int(vals[2]))
            nham_wrong.append(int(vals[3]))
            nham_unsure.append(int(vals[4]))
            nspam_tested.append(int(vals[5]))
            nspam_trained.append(int(vals[6]))
            nspam_right.append(int(vals[7]))
            nspam_wrong.append(int(vals[8]))
            nspam_unsure.append(int(vals[9]))
    outputset(Output)
if __name__ == "__main__":
    main()
