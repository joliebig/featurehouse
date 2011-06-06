import DNS # From http://sourceforge.net/projects/pydns/
import sys
import os
import operator
import time
import types
import socket
try:
    import cPickle as pickle
except ImportError:
    import pickle
from spambayes.Options import options
kCheckForPruneEvery=20
kMaxTTL=60 * 60 * 24 * 7 # One week
kPruneThreshold=1500 # May go over slightly; numbers chosen at random
kPruneDownTo=1000
class lookupResult(object):
    def __init__(self,qType,answer,question,expiresAt,now):
        self.qType=qType
        self.answer=answer
        self.question=question
        self.expiresAt=expiresAt
        self.lastUsed=now
        return None
def sort_by_attr(seq, attr):
    """Sort the sequence of objects by object's attribute
    Arguments:
    seq  - the list or any sequence (including immutable one) of objects to sort.
    attr - the name of attribute to sort by
    Returns:
    the sorted list of objects.
    """
    intermed = map(None, map(getattr, seq, (attr,)*len(seq)), xrange(len(seq)), seq)
    intermed.sort()
    return map(operator.getitem, intermed, (-1,) * len(intermed))
class cache:
    def __init__(self,dnsServer=None,cachefile=""):
        self.printStatsAtEnd=False
        self.returnSinglePTR=True
        self.cacheErrorSecs=5*60
        self.dnsTimeout=10
        self.minTTL=0
        self.cachefile = os.path.expanduser(cachefile)
        if self.cachefile and os.path.exists(self.cachefile):
            self.caches = pickle.load(open(self.cachefile, "rb"))
        else:
            self.caches = {"A": {}, "PTR": {}}
        if options["globals", "verbose"]:
            if self.caches["A"] or self.caches["PTR"]:
                print >> sys.stderr, "opened existing cache with",
                print >> sys.stderr, len(self.caches["A"]), "A records",
                print >> sys.stderr, "and", len(self.caches["PTR"]),
                print >> sys.stderr, "PTR records"
            else:
                print >> sys.stderr, "opened new cache"
        self.hits=0 # These two for statistics
        self.misses=0
        self.pruneTicker=0
        if dnsServer==None:
            DNS.DiscoverNameServers()
            self.queryObj=DNS.DnsRequest()
        else:
            self.queryObj=DNS.DnsRequest(server=dnsServer)
        return None
    def close(self):
        if self.printStatsAtEnd:
            self.printStats()
        if self.cachefile:
            pickle.dump(self.caches, open(self.cachefile, "wb"))
    def printStats(self):
        for key,val in self.caches.items():
            totAnswers=0
            for item in val.values():
                totAnswers+=len(item)
            print >> sys.stderr, "cache", key, "has", len(self.caches[key]),
            print >> sys.stderr, "question(s) and", totAnswers, "answer(s)"
        if self.hits+self.misses==0:
            print >> sys.stderr, "No queries"
        else:
            print >> sys.stderr, self.hits, "hits,", self.misses, "misses",
            print >> sys.stderr, "(%.1f%% hits)" % \
                  (self.hits/float(self.hits+self.misses)*100)
    def prune(self,now):
        allAnswers=[]
        for cache in self.caches.values():
            for val in cache.values():
                allAnswers += val
        allAnswers=sort_by_attr(allAnswers,"expiresAt")
        allAnswers.reverse()
        while True:
            if allAnswers[-1].expiresAt>now:
                break
            answer=allAnswers.pop()
            c=self.caches[answer.qType]
            c[answer.question].remove(answer)
            if len(c[answer.question])==0:
                del c[answer.question]
        if options["globals", "verbose"]:
            self.printStats()
        if len(allAnswers)<=kPruneDownTo:
            return None
        allAnswers=sort_by_attr(allAnswers,"lastUsed")
        allAnswers.reverse()
        numToDelete=len(allAnswers)-kPruneDownTo
        for count in range(numToDelete):
            answer=allAnswers.pop()
            c=self.caches[answer.qType]
            c[answer.question].remove(answer)
            if len(c[answer.question])==0:
                del c[answer.question]
        return None
    def formatForReturn(self,listOfObjs):
        if len(listOfObjs)==1 and listOfObjs[0].answer==None:
            return []
        if listOfObjs[0].qType=="PTR" and self.returnSinglePTR:
            return listOfObjs[0].answer
        return [ obj.answer for obj in listOfObjs ]
    def lookup(self,question,qType="A"):
        qType=qType.upper()
        if qType not in ("A","PTR"):
            raise ValueError,"Query type must be one of A, PTR"
        now=int(time.time())
        self.pruneTicker+=1
        if self.pruneTicker==kCheckForPruneEvery:
            self.pruneTicker=0
            if len(self.caches["A"])+len(self.caches["PTR"])>kPruneThreshold:
                self.prune(now)
        cacheToLookIn=self.caches[qType]
        try:
            answers=cacheToLookIn[question]
        except KeyError:
            pass
        else:
            assert len(answers)>0
            ind=0
            while ind<len(answers):
                thisAnswer=answers[ind]
                if thisAnswer.expiresAt<now:
                    del answers[ind]
                else:
                    thisAnswer.lastUsed=now
                    ind+=1
            if len(answers)==0:
                del cacheToLookIn[question]
            else:
                self.hits+=1
                return self.formatForReturn(answers)
        self.misses+=1
        if qType=="PTR":
            qList=question.split(".")
            qList.reverse()
            queryQuestion=".".join(qList)+".in-addr.arpa"
        else:
            queryQuestion=question
        try:
            reply=self.queryObj.req(queryQuestion,qtype=qType,timeout=self.dnsTimeout)
        except DNS.Base.DNSError,detail:
            if detail.args[0]<>"Timeout":
                print "Error, fixme",detail
                print "Question was",queryQuestion
                print "Origianal question was",question
                print "Type was",qType
            objs=[ lookupResult(qType,None,question,self.cacheErrorSecs+now,now) ]
            cacheToLookIn[question]=objs # Add to format for return?
            return self.formatForReturn(objs)
        except socket.gaierror,detail:
            print "DNS connection failure:", self.queryObj.ns, detail
            print "Defaults:", DNS.defaults
        objs=[]
        for answer in reply.answers:
            if answer["typename"]==qType:
                ttl=max(min(int(answer["ttl"]),kMaxTTL),self.minTTL)
                if ttl>0:
                    item=lookupResult(qType,answer["data"],question,ttl+now,now)
                    objs.append(item)
        if len(objs)>0:
            cacheToLookIn[question]=objs
            return self.formatForReturn(objs)
        if len(reply.authority)==0:
            objs=[ lookupResult(qType,None,question,self.cacheErrorSecs+now,now) ]
            cacheToLookIn[question]=objs
            return self.formatForReturn(objs)
        auth=reply.authority[0]
        auTTL=int(auth["ttl"])
        for item in auth["data"]:
            if type(item)==types.TupleType and item[0]=="minimum":
                auMin=int(item[1])
                cacheNeg=min(auMin,auTTL)
                break
        else:
            cacheNeg=auTTL
        objs=[ lookupResult(qType,None,question,cacheNeg+now,now) ]
        cacheToLookIn[question]=objs
        return self.formatForReturn(objs)
def main():
    import transaction
    c=cache(cachefile=os.path.expanduser("~/.dnscache"))
    c.printStatsAtEnd=True
    for host in ["www.python.org", "www.timsbloggers.com",
                 "www.seeputofor.com", "www.completegarbage.tv",
                 "www.tradelinkllc.com"]:
        print "checking", host
        now=time.time()
        ips=c.lookup(host)
        print ips,time.time()-now
        now=time.time()
        ips=c.lookup(host)
        print ips,time.time()-now
        if ips:
            ip=ips[0]
            now=time.time()
            name=c.lookup(ip,qType="PTR")
            print name,time.time()-now
            now=time.time()
            name=c.lookup(ip,qType="PTR")
            print name,time.time()-now
        else:
            print "unknown"
    c.close()
    return None
if __name__=="__main__":
    main()
