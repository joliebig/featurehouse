package org.prevayler.implementation.publishing;
import org.prevayler.implementation.publishing.censorship.TransactionCensor;
public class CentralPublisher {
  private TransactionCensor _censor;
  CentralPublisher(  Clock clock,  TransactionCensor censor,  Journal journal){
    this(clock,journal);
    _censor=censor;
  }
  protected void hook79(  TransactionTimestamp timestamp){
    _censor.approve(timestamp);
    original(timestamp);
  }
}
