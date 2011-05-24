package org.prevayler.implementation.publishing;
import org.prevayler.Clock;
import org.prevayler.foundation.Cool;
import org.prevayler.foundation.Turn;
import org.prevayler.implementation.Capsule;
import org.prevayler.implementation.TransactionGuide;
import org.prevayler.implementation.TransactionTimestamp;
import org.prevayler.implementation.clock.PausableClock;
import org.prevayler.implementation.journal.Journal;
import java.io.IOException;

public class CentralPublisher extends AbstractPublisher {
  private final PausableClock _pausableClock;
  private final Journal _journal;
  private volatile int _pendingPublications=0;
  private final Object _pendingPublicationsMonitor=new Object();
  private Turn _nextTurn=Turn.first();
  private long _nextTransaction;
  private final Object _nextTurnMonitor=new Object();
  public CentralPublisher(  Clock clock,  Journal journal){
    super(new PausableClock(clock));
    _pausableClock=(PausableClock)_clock;
    _journal=journal;
  }
  public void publish(  Capsule capsule){
synchronized (_pendingPublicationsMonitor) {
      if (_pendingPublications == 0)       _pausableClock.pause();
      _pendingPublications++;
    }
    try {
      publishWithoutWorryingAboutNewSubscriptions(capsule);
    }
  finally {
synchronized (_pendingPublicationsMonitor) {
        _pendingPublications--;
        if (_pendingPublications == 0) {
          _pausableClock.resume();
          _pendingPublicationsMonitor.notifyAll();
        }
      }
    }
  }
  private void publishWithoutWorryingAboutNewSubscriptions(  Capsule capsule){
    TransactionGuide guide=approve(capsule);
    _journal.append(guide);
    notifySubscribers(guide);
  }
  private TransactionGuide approve(  Capsule capsule){
synchronized (_nextTurnMonitor) {
      TransactionTimestamp timestamp=new TransactionTimestamp(capsule,_nextTransaction,_pausableClock.realTime());
      this.hook79(timestamp);
      Turn turn=_nextTurn;
      _nextTurn=_nextTurn.next();
      _nextTransaction++;
      return new TransactionGuide(timestamp,turn);
    }
  }
  private void notifySubscribers(  TransactionGuide guide){
    guide.startTurn();
    try {
      _pausableClock.advanceTo(guide.executionTime());
      notifySubscribers(guide.timestamp());
    }
  finally {
      guide.endTurn();
    }
  }
  public void subscribe(  TransactionSubscriber subscriber,  long initialTransaction) throws IOException, ClassNotFoundException {
synchronized (_pendingPublicationsMonitor) {
      while (_pendingPublications != 0)       Cool.wait(_pendingPublicationsMonitor);
      _journal.update(subscriber,initialTransaction);
synchronized (_nextTurnMonitor) {
        _nextTransaction=_journal.nextTransaction();
      }
      super.addSubscriber(subscriber);
    }
  }
  public void close() throws IOException {
    _journal.close();
  }
  protected void hook79(  TransactionTimestamp timestamp){
  }
}
