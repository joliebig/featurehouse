from spambayes.Options import options
class CostCounter:
    name = "Superclass Cost"
    def __init__(self):
        self.total = 0
    def spam(self, scr):
        pass
    def ham(self, scr):
        pass
    def __str__(self):
        return "%s: $%.4f" % (self.name, self.total)
class CompositeCostCounter:
    def __init__(self, cclist):
        self.clients = cclist
    def spam(self, scr):
        for c in self.clients:
            c.spam(scr)
    def ham(self, scr):
        for c in self.clients:
            c.ham(scr)
    def __str__(self):
        s = []
        for c in self.clients:
            s.append(str(c))
        return '\n'.join(s)
class DelayedCostCounter(CompositeCostCounter):
    def __init__(self, cclist):
        CompositeCostCounter.__init__(self, cclist)
        self.spamscr = []
        self.hamscr = []
    def spam(self, scr):
        self.spamscr.append(scr)
    def ham(self, scr):
        self.hamscr.append(scr)
    def __str__(self):
        for scr in self.spamscr:
            CompositeCostCounter.spam(self, scr)
        for scr in self.hamscr:
            CompositeCostCounter.ham(self, scr)
        s = []
        for line in CompositeCostCounter.__str__(self).split('\n'):
            s.append('Delayed-'+line)
        return '\n'.join(s)
class CountCostCounter(CostCounter):
    def __init__(self):
        CostCounter.__init__(self)
        self._fp = 0
        self._fn = 0
        self._unsure = 0
        self._unsureham = 0
        self._unsurespam = 0
        self._spam = 0
        self._ham = 0
        self._correctham = 0
        self._correctspam = 0
        self._total = 0
    def spam(self, scr):
        self._total += 1
        self._spam += 1
        if scr < options["Categorization", "ham_cutoff"]:
            self._fn += 1
        elif scr < options["Categorization", "spam_cutoff"]:
            self._unsure += 1
            self._unsurespam += 1
        else:
            self._correctspam += 1
    def ham(self, scr):
        self._total += 1
        self._ham += 1
        if scr > options["Categorization", "spam_cutoff"]:
            self._fp += 1
        elif scr > options["Categorization", "ham_cutoff"]:
            self._unsure += 1
            self._unsureham += 1
        else:
            self._correctham += 1
    def __str__(self):
        return ("Total messages: %d; %d (%.1f%%) ham + %d (%.1f%%) spam\n"%(
                    self._total,
                    self._ham, zd(100.*self._ham,self._total),
                    self._spam, zd(100.*self._spam,self._total))+
                "Ham: %d (%.2f%%) ok, %d (%.2f%%) unsure, %d (%.2f%%) fp\n"%(
                    self._correctham, zd(100.*self._correctham,self._ham),
                    self._unsureham, zd(100.*self._unsureham,self._ham),
                    self._fp, zd(100.*self._fp,self._ham))+
                "Spam: %d (%.2f%%) ok, %d (%.2f%%) unsure, %d (%.2f%%) fn\n"%(
                    self._correctspam, zd(100.*self._correctspam,self._spam),
                    self._unsurespam, zd(100.*self._unsurespam,self._spam),
                    self._fn, zd(100.*self._fn,self._spam))+
                "Score False: %.2f%% Unsure %.2f%%"%(
                    zd(100.*(self._fp+self._fn),self._total),
                    zd(100.*self._unsure,self._total)))
def zd(x, y):
    if y > 0:
        return x / y
    else:
        return 0
class StdCostCounter(CostCounter):
    name = "Standard Cost"
    def spam(self, scr):
        if scr < options["Categorization", "ham_cutoff"]:
            self.total += options["TestDriver", "best_cutoff_fn_weight"]
        elif scr < options["Categorization", "spam_cutoff"]:
            self.total += options["TestDriver", "best_cutoff_unsure_weight"]
    def ham(self, scr):
        if scr > options["Categorization", "spam_cutoff"]:
            self.total += options["TestDriver", "best_cutoff_fp_weight"]
        elif scr > options["Categorization", "ham_cutoff"]:
            self.total += options["TestDriver", "best_cutoff_unsure_weight"]
class FlexCostCounter(CostCounter):
    name = "Flex Cost"
    def _lambda(self, scr):
        if scr < options["Categorization", "ham_cutoff"]:
            return 0
        elif scr > options["Categorization", "spam_cutoff"]:
            return 1
        else:
            return (scr - options["Categorization", "ham_cutoff"]) / (
                      options["Categorization", "spam_cutoff"] \
                      - options["Categorization", "ham_cutoff"])
    def spam(self, scr):
        self.total += (1 - self._lambda(scr)) * options["TestDriver",
                                                        "best_cutoff_fn_weight"]
    def ham(self, scr):
        self.total += self._lambda(scr) * options["TestDriver",
                                                  "best_cutoff_fp_weight"]
class Flex2CostCounter(FlexCostCounter):
    name = "Flex**2 Cost"
    def spam(self, scr):
        self.total += (1 - self._lambda(scr))**2 * options["TestDriver",
                                                           "best_cutoff_fn_weight"]
    def ham(self, scr):
        self.total += self._lambda(scr)**2 * options["TestDriver",
                                                     "best_cutoff_fp_weight"]
def default():
    return CompositeCostCounter([
               CountCostCounter(),
               StdCostCounter(),
               FlexCostCounter(),
               Flex2CostCounter(),
               DelayedCostCounter([
                   CountCostCounter(),
                   StdCostCounter(),
                   FlexCostCounter(),
                   Flex2CostCounter(),
               ])
           ])
def nodelay():
    return CompositeCostCounter([
               CountCostCounter(),
               StdCostCounter(),
               FlexCostCounter(),
               Flex2CostCounter(),
           ])
if __name__ == "__main__":
    cc = default()
    cc.ham(0)
    cc.spam(1)
    cc.ham(0.5)
    cc.spam(0.5)
    options["Categorization", "spam_cutoff"] = 0.7
    options["Categorization", "ham_cutoff"] = 0.4
    print(cc)
