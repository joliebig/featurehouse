"""setup.py
Generate any dynamic documentation for the Outlook plug-in.
Typically, this involves options that can be set - dynamic generation means
that the information needs only be updated in one central location, and the
documentation can still stay up-to-date.
"""
__author__ = "Tony Meyer <ta-meyer@ihug.co.nz>"
__credits__ = "All the spambayes folk."
import os
import sys
sys.path.append(os.path.dirname(os.path.dirname(__file__)))
import config
from spambayes.Options import defaults
from spambayes.OptionsClass import OptionsClass
from spambayes.OptionsClass import PATH, INTEGER, REAL, HEADER_NAME
nice_regex_names = {PATH : "Filename",
                    INTEGER : "Whole number",
                    REAL : "Number",
                    HEADER_NAME : "Email Header Name",
                    config.FOLDER_ID : None,
                    config.FIELD_NAME : "Alphanumeric characters",
                    }
table_header = """
    <!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
    <html>
    <head>
      <meta http-equiv="content-type"
     content="text/html; charset=ISO-8859-1">
      <title>Available options</title>
    <link rel='stylesheet' type="text/css" href="style.css" title="Intended style" />
    </head>
    <body>
    <table width="100%" cellspacing="0" cellpadding="0">
        <tr>
          <td width="380"><img src="images/sblogo.jpg" title=""
     alt="Logo" style="width: 380px; height: 77px;"></td>
    <!-- use background instead of styles to avoid mozilla bug 167262 --> <td
     background="images/span.jpg">&nbsp;</td>
          <td width="78"><img src="images/python.jpg"
     style="width: 78px; height: 77px;"></td>
        </tr>
    </table>
    <h1>Available options</h1>
    <table border='1' cellpadding='2' cellspacing='2' class="options">
      <tr class="options-heading">
        <td>Section</td>
        <td>Option Name</td>
        <td>Valid Values</td>
        <td>Default</td>
        <td>Comments</td>
      </tr>
"""
def main():
    outlook_config = config.CreateConfig()
    spambayes_config = OptionsClass()
    spambayes_config.load_defaults(defaults)
    for fn, o, sects in [("outlook-options.html", outlook_config,
                          ("General", "Filter", "Training", "Notification")),
                         ("spambayes-options.html", spambayes_config,
                          ("Tokenizer", "General", "Classifier", "Storage"))]:
        f = open(fn, "w")
        f.write(table_header)
        for sect in sects:
            f.write('<tr style="height:1em">&nbsp;</tr>\n')
            opts = o.options_in_section(sect)
            opts.sort()
            for opt_name in opts:
                opt = o.get_option(sect, opt_name)
                if opt_name.startswith("x-"):
                    continue
                if opt.allowed_values in nice_regex_names:
                    replacement = nice_regex_names[opt.allowed_values]
                    if replacement is None:
                        continue
                    opt.allowed_values = (replacement,)
                f.write(opt.as_documentation_string(sect))
                f.write('\n')
        f.write("</table>\n")
        f.close()
    for fn, o in (("outlook-defaults.ini", outlook_config),
                  ("spambayes-defaults.ini", spambayes_config)):
        f = open(fn, "w")
        f.write(o.display(add_comments=True))
        f.close()
if __name__ == "__main__":
    main()
