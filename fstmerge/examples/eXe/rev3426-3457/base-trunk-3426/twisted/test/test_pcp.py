__version__ = '$Revision: 1.5 $'[11:-2]
from StringIO import StringIO
from twisted.trial import unittest
from twisted.protocols import pcp
class DummyTransport:
    """A dumb transport to wrap around."""
    def __init__(self):
        self._writes = []
    def write(self, data):
        self._writes.append(data)
    def getvalue(self):
        return ''.join(self._writes)
class DummyProducer:
    resumed = False
    stopped = False
    paused = False
    def __init__(self, consumer):
        self.consumer = consumer
    def resumeProducing(self):
        self.resumed = True
        self.paused = False
    def pauseProducing(self):
        self.paused = True
    def stopProducing(self):
        self.stopped = True
class DummyConsumer(DummyTransport):
    producer = None
    finished = False
    unregistered = True
    def registerProducer(self, producer, streaming):
        self.producer = (producer, streaming)
    def unregisterProducer(self):
        self.unregistered = True
    def finish(self):
        self.finished = True
class TransportInterfaceTest(unittest.TestCase):
    proxyClass = pcp.BasicProducerConsumerProxy
    def setUp(self):
        self.underlying = DummyConsumer()
        self.transport = self.proxyClass(self.underlying)
    def testWrite(self):
        self.transport.write("some bytes")
class ConsumerInterfaceTest:
    """Test ProducerConsumerProxy as a Consumer.
    Normally we have ProducingServer -> ConsumingTransport.
    If I am to go between (Server -> Shaper -> Transport), I have to
    play the role of Consumer convincingly for the ProducingServer.
    """
    def setUp(self):
        self.underlying = DummyConsumer()
        self.consumer = self.proxyClass(self.underlying)
        self.producer = DummyProducer(self.consumer)
    def testRegisterPush(self):
        self.consumer.registerProducer(self.producer, True)
        self.failIf(self.producer.resumed)
    def testUnregister(self):
        self.consumer.registerProducer(self.producer, False)
        self.consumer.unregisterProducer()
        self.producer.resumed = False
        self.consumer.resumeProducing()
        self.failIf(self.producer.resumed)
    def testFinish(self):
        self.consumer.registerProducer(self.producer, False)
        self.consumer.finish()
        self.producer.resumed = False
        self.consumer.resumeProducing()
        self.failIf(self.producer.resumed)
class ProducerInterfaceTest:
    """Test ProducerConsumerProxy as a Producer.
    Normally we have ProducingServer -> ConsumingTransport.
    If I am to go between (Server -> Shaper -> Transport), I have to
    play the role of Producer convincingly for the ConsumingTransport.
    """
    def setUp(self):
        self.consumer = DummyConsumer()
        self.producer = self.proxyClass(self.consumer)
    def testRegistersProducer(self):
        self.failUnlessEqual(self.consumer.producer[0], self.producer)
    def testPause(self):
        self.producer.pauseProducing()
        self.producer.write("yakkity yak")
        self.failIf(self.consumer.getvalue(),
                    "Paused producer should not have sent data.")
    def testResume(self):
        self.producer.pauseProducing()
        self.producer.resumeProducing()
        self.producer.write("yakkity yak")
        self.failUnlessEqual(self.consumer.getvalue(), "yakkity yak")
    def testResumeNoEmptyWrite(self):
        self.producer.pauseProducing()
        self.producer.resumeProducing()
        self.failUnlessEqual(len(self.consumer._writes), 0,
                             "Resume triggered an empty write.")
    def testResumeBuffer(self):
        self.producer.pauseProducing()
        self.producer.write("buffer this")
        self.producer.resumeProducing()
        self.failUnlessEqual(self.consumer.getvalue(), "buffer this")
    def testStop(self):
        self.producer.stopProducing()
        self.producer.write("yakkity yak")
        self.failIf(self.consumer.getvalue(),
                    "Stopped producer should not have sent data.")
class PCP_ConsumerInterfaceTest(ConsumerInterfaceTest, unittest.TestCase):
    proxyClass = pcp.BasicProducerConsumerProxy
class PCPII_ConsumerInterfaceTest(ConsumerInterfaceTest, unittest.TestCase):
    proxyClass = pcp.ProducerConsumerProxy
class PCP_ProducerInterfaceTest(ProducerInterfaceTest, unittest.TestCase):
    proxyClass = pcp.BasicProducerConsumerProxy
class PCPII_ProducerInterfaceTest(ProducerInterfaceTest, unittest.TestCase):
    proxyClass = pcp.ProducerConsumerProxy
class ProducerProxyTest(unittest.TestCase):
    """Producer methods on me should be relayed to the Producer I proxy.
    """
    proxyClass = pcp.BasicProducerConsumerProxy
    def setUp(self):
        self.proxy = self.proxyClass(None)
        self.parentProducer = DummyProducer(self.proxy)
        self.proxy.registerProducer(self.parentProducer, True)
    def testStop(self):
        self.proxy.stopProducing()
        self.failUnless(self.parentProducer.stopped)
class ConsumerProxyTest(unittest.TestCase):
    """Consumer methods on me should be relayed to the Consumer I proxy.
    """
    proxyClass = pcp.BasicProducerConsumerProxy
    def setUp(self):
        self.underlying = DummyConsumer()
        self.consumer = self.proxyClass(self.underlying)
    def testWrite(self):
        self.consumer.write("some bytes")
        self.failUnlessEqual(self.underlying.getvalue(), "some bytes")
    def testFinish(self):
        self.consumer.finish()
        self.failUnless(self.underlying.finished)
    def testUnregister(self):
        self.consumer.unregisterProducer()
        self.failUnless(self.underlying.unregistered)
class PullProducerTest:
    def setUp(self):
        self.underlying = DummyConsumer()
        self.proxy = self.proxyClass(self.underlying)
        self.parentProducer = DummyProducer(self.proxy)
        self.proxy.registerProducer(self.parentProducer, True)
    def testHoldWrites(self):
        self.proxy.write("hello")
        self.failIf(self.underlying.getvalue(),
                    "Pulling Consumer got data before it pulled.")
    def testPull(self):
        self.proxy.write("hello")
        self.proxy.resumeProducing()
        self.failUnlessEqual(self.underlying.getvalue(), "hello")
    def testMergeWrites(self):
        self.proxy.write("hello ")
        self.proxy.write("sunshine")
        self.proxy.resumeProducing()
        nwrites = len(self.underlying._writes)
        self.failUnlessEqual(nwrites, 1, "Pull resulted in %d writes instead "
                             "of 1." % (nwrites,))
        self.failUnlessEqual(self.underlying.getvalue(), "hello sunshine")
    def testLateWrite(self):
        self.proxy.resumeProducing()
        self.proxy.write("data")
        self.failUnlessEqual(self.underlying.getvalue(), "data")
class PCP_PullProducerTest(PullProducerTest, unittest.TestCase):
    class proxyClass(pcp.BasicProducerConsumerProxy):
        iAmStreaming = False
class PCPII_PullProducerTest(PullProducerTest, unittest.TestCase):
    class proxyClass(pcp.ProducerConsumerProxy):
        iAmStreaming = False
class BufferedConsumerTest(unittest.TestCase):
    """As a consumer, ask the producer to pause after too much data."""
    proxyClass = pcp.ProducerConsumerProxy
    def setUp(self):
        self.underlying = DummyConsumer()
        self.proxy = self.proxyClass(self.underlying)
        self.proxy.bufferSize = 100
        self.parentProducer = DummyProducer(self.proxy)
        self.proxy.registerProducer(self.parentProducer, True)
    def testRegisterPull(self):
        self.proxy.registerProducer(self.parentProducer, False)
        self.failUnless(self.parentProducer.resumed)
    def testPauseIntercept(self):
        self.proxy.pauseProducing()
        self.failIf(self.parentProducer.paused)
    def testResumeIntercept(self):
        self.proxy.pauseProducing()
        self.proxy.resumeProducing()
        self.failIf(self.parentProducer.resumed)
    def testTriggerPause(self):
        """Make sure I say \"when.\""""
        self.proxy.pauseProducing()
        self.failIf(self.parentProducer.paused, "don't pause yet")
        self.proxy.write("x" * 51)
        self.failIf(self.parentProducer.paused, "don't pause yet")
        self.proxy.write("x" * 51)
        self.failUnless(self.parentProducer.paused)
    def testTriggerResume(self):
        """Make sure I resumeProducing when my buffer empties."""
        self.proxy.pauseProducing()
        self.proxy.write("x" * 102)
        self.failUnless(self.parentProducer.paused, "should be paused")
        self.proxy.resumeProducing()
        self.failIf(self.parentProducer.paused,
                    "Producer should have resumed.")
        self.failIf(self.proxy.producerPaused)
class BufferedPullTests(unittest.TestCase):
    class proxyClass(pcp.ProducerConsumerProxy):
        iAmStreaming = False
        def _writeSomeData(self, data):
            pcp.ProducerConsumerProxy._writeSomeData(self, data[:100])
            return min(len(data), 100)
    def setUp(self):
        self.underlying = DummyConsumer()
        self.proxy = self.proxyClass(self.underlying)
        self.proxy.bufferSize = 100
        self.parentProducer = DummyProducer(self.proxy)
        self.proxy.registerProducer(self.parentProducer, False)
    def testResumePull(self):
        self.parentProducer.resumed = False
        self.proxy.resumeProducing()
        self.failUnless(self.parentProducer.resumed)
    def testLateWriteBuffering(self):
        self.proxy.resumeProducing()
        self.proxy.write("datum" * 21)
        self.failUnlessEqual(self.underlying.getvalue(), "datum" * 20)
        self.failUnlessEqual(self.proxy._buffer, ["datum"])
