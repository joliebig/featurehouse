import DNS
import sys
import os
import operator
import time
import types
import socket
from spambayes.Options import options
from spambayes.safepickle import pickle_read, pickle_write
kCheckForPruneEvery = 20
kMaxTTL = 60 * 60 * 24 * 7                # One week
kMinTTL = 24 * 60 * 60 * 1                # one day
kPruneThreshold = 5000 # May go over slightly; numbers chosen at random
kPruneDownTo = 2500
class lookupResult(object):
    def __init__(self, qType, answer, question, expiresAt, now):
        self.qType = qType
        self.answer = answer
        self.question = question
        self.expiresAt = expiresAt
        self.lastUsed = now
        return None
def sort_by_attr(seq, attr):
    """Sort the sequence of objects by object's attribute
    Arguments:
    seq  - the list or any sequence (including immutable one) of objects to sort.
    attr - the name of attribute to sort by
    Returns:
    the sorted list of objects.
    """
    intermed = list(zip([getattr(s, attr) for s in seq], range(len(seq)), seq))
    intermed.sort()
    return list(map(operator.getitem, intermed, (-1,) * len(intermed)))
class cache:
    def __init__(self, dnsServer=None, cachefile=""):
        self.printStatsAtEnd = False
        self.returnSinglePTR = True
        self.cacheErrorSecs=5*60
        self.dnsTimeout=10
        self.cachefile = os.path.expanduser(cachefile)
        self.caches = None
        if self.cachefile and os.path.exists(self.cachefile):
            try:
                self.caches = pickle_read(self.cachefile)
            except:
                os.unlink(self.cachefile)
        if self.caches is None:
            self.caches = {"A": {}, "PTR": {}}
        if options["globals", "verbose"]:
            if self.caches["A"] or self.caches["PTR"]:
                print("opened existing cache with", end=' ', file=sys.stderr)
                print(len(self.caches["A"]), "A records", end=' ', file=sys.stderr)
                print("and", len(self.caches["PTR"]), end=' ', file=sys.stderr)
                print("PTR records", file=sys.stderr)
            else:
                print("opened new cache", file=sys.stderr)
        self.hits=0 # These two for statistics
        self.misses=0
        self.pruneTicker=0
        if dnsServer == None:
            DNS.DiscoverNameServers()
            self.queryObj = DNS.DnsRequest()
        else:
            self.queryObj = DNS.DnsRequest(server=dnsServer)
        return None
    def close(self):
        if self.printStatsAtEnd:
            self.printStats()
        if self.cachefile:
            pickle_write(self.cachefile, self.caches)
    def printStats(self):
        for key,val in list(self.caches.items()):
            totAnswers=0
            for item in list(val.values()):
                totAnswers+=len(item)
            print("cache", key, "has", len(self.caches[key]), end=' ', file=sys.stderr)
            print("question(s) and", totAnswers, "answer(s)", file=sys.stderr)
        if self.hits+self.misses == 0:
            print("No queries", file=sys.stderr)
        else:
            print(self.hits, "hits,", self.misses, "misses", end=' ', file=sys.stderr)
            print("(%.1f%% hits)" % \
                  (self.hits/float(self.hits+self.misses)*100), file=sys.stderr)
    def prune(self, now):
        allAnswers = []
        for cache in list(self.caches.values()):
            for val in list(cache.values()):
                allAnswers += val
        allAnswers = sort_by_attr(allAnswers,"expiresAt")
        allAnswers.reverse()
        while True:
            if allAnswers[-1].expiresAt > now:
                break
            answer = allAnswers.pop()
            c = self.caches[answer.qType]
            c[answer.question].remove(answer)
            if  not c[answer.question]:
                del c[answer.question]
        if options["globals", "verbose"]:
            self.printStats()
        if len(allAnswers)<=kPruneDownTo:
            return None
        allAnswers = sort_by_attr(allAnswers, "lastUsed")
        allAnswers.reverse()
        numToDelete = len(allAnswers)-kPruneDownTo
        for _count in range(numToDelete):
            answer = allAnswers.pop()
            c = self.caches[answer.qType]
            c[answer.question].remove(answer)
            if not c[answer.question]:
                del c[answer.question]
        return None
    def formatForReturn(self, listOfObjs):
        if len(listOfObjs) == 1 and listOfObjs[0].answer == None:
            return []
        if listOfObjs[0].qType == "PTR" and self.returnSinglePTR:
            return listOfObjs[0].answer
        return [ obj.answer for obj in listOfObjs ]
    def lookup(self,question,qType="A"):
        qType = qType.upper()
        if qType not in ("A","PTR"):
            raise ValueError("Query type must be one of A, PTR")
        now = int(time.time())
        self.pruneTicker += 1
        if self.pruneTicker == kCheckForPruneEvery:
            self.pruneTicker = 0
            if len(self.caches["A"])+len(self.caches["PTR"])>kPruneThreshold:
                self.prune(now)
        cacheToLookIn = self.caches[qType]
        try:
            answers = cacheToLookIn[question]
        except KeyError:
            pass
        else:
            if answers:
                ind = 0
                while ind<len(answers):
                    thisAnswer = answers[ind]
                    if thisAnswer.expiresAt<now:
                        del answers[ind]
                    else:
                        thisAnswer.lastUsed = now
                        ind += 1
            else:
                print("lookup failure:", question, file=sys.stderr)
            if not answers:
                del cacheToLookIn[question]
            else:
                self.hits += 1
                return self.formatForReturn(answers)
        self.misses += 1
        if qType == "PTR":
            qList = question.split(".")
            qList.reverse()
            queryQuestion = ".".join(qList)+".in-addr.arpa"
        else:
            queryQuestion = question
        try:
            reply = self.queryObj.req(queryQuestion, qtype=qType,
                                      timeout=self.dnsTimeout)
        except DNS.Base.DNSError as detail:
            if detail.args[0] not in ("Timeout", "nothing to lookup"):
                print(detail.args[0], file=sys.stderr)
                print("Error, fixme", detail, file=sys.stderr)
                print("Question was", queryQuestion, file=sys.stderr)
                print("Original question was", question, file=sys.stderr)
                print("Type was", qType, file=sys.stderr)
            objs = [lookupResult(qType, None, question,
                                 self.cacheErrorSecs+now, now)]
            cacheToLookIn[question] = objs # Add to format for return?
            return self.formatForReturn(objs)
        except socket.gaierror as detail:
            print("DNS connection failure:", self.queryObj.ns, detail, file=sys.stderr)
            print("Defaults:", DNS.defaults, file=sys.stderr)
        objs = []
        for answer in reply.answers:
            if answer["typename"] == qType:
                ttl = max(min(int(answer["ttl"]), kMaxTTL), kMinTTL)
                if ttl > 0:
                    item = lookupResult(qType, answer["data"], question,
                                        ttl+now, now)
                    objs.append(item)
        if objs:
            cacheToLookIn[question] = objs
            return self.formatForReturn(objs)
        if not reply.authority:
            objs = [lookupResult(qType, None, question,
                                 self.cacheErrorSecs+now, now)]
            cacheToLookIn[question] = objs
            return self.formatForReturn(objs)
        auth = reply.authority[0]
        auTTL = int(auth["ttl"])
        for item in auth["data"]:
            if type(item) == tuple and item[0] == "minimum":
                auMin = int(item[1])
                cacheNeg = min(auMin,auTTL)
                break
        else:
            cacheNeg = auTTL
        objs = [lookupResult(qType, None, question, cacheNeg+now, now)]
        cacheToLookIn[question] = objs
        return self.formatForReturn(objs)
def main():
    import transaction
    c = cache(cachefile=os.path.expanduser("~/.dnscache"))
    c.printStatsAtEnd = True
    for host in ["www.python.org", "www.timsbloggers.com",
                 "www.seeputofor.com", "www.completegarbage.tv",
                 "www.tradelinkllc.com"]:
        print("checking", host, file=sys.stderr)
        now = time.time()
        ips = c.lookup(host)
        print(ips, time.time()-now, file=sys.stderr)
        now = time.time()
        ips = c.lookup(host)
        print(ips, time.time()-now, file=sys.stderr)
        if ips:
            ip = ips[0]
            now = time.time()
            name = c.lookup(ip, qType="PTR")
            print(name, time.time()-now, file=sys.stderr)
            now = time.time()
            name = c.lookup(ip, qType="PTR")
            print(name, time.time()-now, file=sys.stderr)
        else:
            print("unknown", file=sys.stderr)
    c.close()
    return None
if __name__ == "__main__":
    main()
