"""Implement just enough of a csv parser to support sb_dbexpimp.py's needs."""
import sys
import re
if sys.platform == "windows":
    EOL = "\r\n"
elif sys.platform == "mac":
    EOL = "\r"
else:
    EOL = "\n"
class reader:
    def __init__(self, fp):
        self.fp = fp
    def __iter__(self):
        return self
    def _readline(self):
        line = self.fp.readline()
        while line[-1:] in ("\r", "\n"):
            line = line[:-1]
        return line
    def next(self):
        line = self._readline()
        if not line:
            raise StopIteration
        return self.parse_line(line)
    def parse_line(self, line):
        """parse the line.
        very simple assumptions:
        * separator is a comma
        * fields are only quoted with quotation marks and only
          quoted if the field contains a comma or a quotation mark
        * embedded quotation marks are doubled
        """
        result = []
        while line:
            if line[0] == '"':
                line = line[1:]
                field = []
                while True:
                    if line[0:2] == '""':
                        field.append('"')
                        line = line[2:]
                    elif line[0] == '"':
                        line = line[1:]
                        if line[0:1] == ',':
                            line = line[1:]
                        break
                    else:
                        field.append(line[0])
                        line = line[1:]
                    if not line:
                        field.append("\n")
                        line = self._readline()
                        if not line:
                            raise IOError, "end-of-file during parsing"
            else:
                field = []
                while line:
                    if line[0] == ',':
                        line = line[1:]
                        break
                    else:
                        field.append(line[0])
                        line = line[1:]
            result.append("".join(field))
        return result
class writer:
    def __init__(self, fp):
        self.fp = fp
    def writerow(self, row):
        result = []
        for item in row:
            if isinstance(item, unicode):
                item = item.encode("utf-8")
            else:
                item = str(item)
            if re.search('["\n,]', item) is not None:
                item = '"%s"' % item.replace('"', '""')
            result.append(item)
        result = ",".join(result)
        self.fp.write(result+EOL)
if __name__ == "__main__":
    import unittest
    import StringIO
    class TestCase(unittest.TestCase):
        def test_reader(self):
            f = StringIO.StringIO('''\
"""rare""",1,0
"beginning;
	end=""itinhh.txt""",1,0
''')
            f.seek(0)
            rdr = reader(f)
            self.assertEqual(rdr.next(), ['"rare"', '1', '0'])
            self.assertEqual(rdr.next(),
                             ['beginning;\n\tend="itinhh.txt"','1', '0'])
            self.assertRaises(StopIteration, rdr.next)
    unittest.main()
