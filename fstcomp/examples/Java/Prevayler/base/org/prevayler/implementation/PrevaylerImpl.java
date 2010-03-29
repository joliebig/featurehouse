package org.prevayler.implementation;
import org.prevayler.Clock;
import org.prevayler.Prevayler;
import org.prevayler.Query;
import org.prevayler.SureTransactionWithQuery;
import org.prevayler.Transaction;
import org.prevayler.TransactionWithQuery;
import org.prevayler.foundation.serialization.Serializer;
import org.prevayler.implementation.publishing.TransactionPublisher;
import java.io.IOException;

public class PrevaylerImpl implements Prevayler {
  private final Clock _clock;
  private final TransactionPublisher _publisher;
  private final Serializer _journalSerializer;
  public PrevaylerImpl(  TransactionPublisher transactionPublisher,  Serializer journalSerializer) throws IOException, ClassNotFoundException {
    _publisher=transactionPublisher;
    _clock=_publisher.clock();
    _journalSerializer=journalSerializer;
  }
  public Clock clock(){
    return _clock;
  }
  public void execute(  Transaction transaction){
    publish(new TransactionCapsule(transaction,_journalSerializer));
  }
  private void publish(  Capsule capsule){
    _publisher.publish(capsule);
  }
  public Object execute(  TransactionWithQuery transactionWithQuery) throws Exception {
    TransactionWithQueryCapsule capsule=new TransactionWithQueryCapsule(transactionWithQuery,_journalSerializer);
    publish(capsule);
    return capsule.result();
  }
  public Object execute(  SureTransactionWithQuery sureTransactionWithQuery){
    try {
      return execute((TransactionWithQuery)sureTransactionWithQuery);
    }
 catch (    RuntimeException runtime) {
      throw runtime;
    }
catch (    Exception checked) {
      throw new RuntimeException("Unexpected Exception thrown.",checked);
    }
  }
  public void close() throws IOException {
    _publisher.close();
  }
}
