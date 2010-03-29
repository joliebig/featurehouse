package org.prevayler.implementation.publishing;
import org.prevayler.implementation.TransactionTimestamp;

public interface TransactionSubscriber {
  public void receive(  TransactionTimestamp transactionTimestamp);
}
