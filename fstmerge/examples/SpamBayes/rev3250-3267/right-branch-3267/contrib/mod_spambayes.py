from proxy3_filter import *
import proxy3_options
from spambayes import hammie, Options
dbf = Options.get_pathname_option("Storage", "persistent_storage_file")
class SpambayesFilter(BufferAllFilter):
    checker = hammie.open(dbf, 1, 'r')
    def filter(self, s):
        if self.reply.split()[1] == '200':
            prob = self.checker.score("%s\r\n%s" % (self.serverheaders, s))
            print("|  prob: %.5f" % prob)
            if prob >= Options.options["Categorization", "spam_cutoff"]:
                print(self.serverheaders)
                print("text:", s[0:40], "...", s[-40:])
                return "not authorized"
        return s
from proxy3_util import *
register_filter('*/*', 'text/html', SpambayesFilter)
