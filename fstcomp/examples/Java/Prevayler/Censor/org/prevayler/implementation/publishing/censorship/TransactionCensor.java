package org.prevayler.implementation.publishing.censorship;
import org.prevayler.implementation.TransactionTimestamp;

public interface TransactionCensor {
  public void approve(  TransactionTimestamp transactionTimestamp) throws RuntimeException, Error ;
}
