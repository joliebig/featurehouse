__author__="Adam Walker"
__doc__=""""
Pulls labels and captions out of a windows resource file
and writes them into a text file for spell checking purposes.
"""
import sys, os, re
import rcparser
anti_and = re.compile(r"([^\\]*)&([^&]*)");
anti_nl = re.compile(r"([^\\]*)\\n([^\\])");
def extract(inputFilename = None, outputFilename = None):
    """See the module doc string"""
    if inputFilename is None:
        inputFilename = "dialogs.rc"
    if outputFilename is None:
        outputFilename = "spellcheck.txt"
    rcp = rcparser.ParseDialogs(inputFilename)
    out = open(outputFilename, "wt")
    for dlg_id in rcp._dialogs:
        print dlg_id
        dlg = rcp._dialogs[dlg_id]
        out.write("\n================================================\n")
        out.write("In Dialog: "+str(dlg_id)+" Title: "+str(dlg.caption)+"\n\n")
        for ctrl in dlg.controls:
            if len(ctrl.label)>0:
                out.write(ctrl.id)
                out.write("\n")
                s = ctrl.label
                s = anti_and.sub(r"\g<1>\g<2>", s)
                s = anti_nl.sub("\\g<1>\n\\g<2>",s)
                out.write(s)
                out.write("\n\n")
    out.close()
    os.startfile(outputFilename);
if __name__=="__main__":
    if len(sys.argv)>1:
        extract(sys.argv[1], sys.argv[2])
    else:
        extract()
