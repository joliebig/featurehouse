package org.prevayler.implementation.publishing.censorship;
import org.prevayler.foundation.serialization.Serializer;
import org.prevayler.implementation.PrevalentSystemGuard;
import org.prevayler.implementation.TransactionTimestamp;
import org.prevayler.implementation.snapshot.GenericSnapshotManager;

public class StrictTransactionCensor implements TransactionCensor {
  private final PrevalentSystemGuard _king;
  private PrevalentSystemGuard _royalFoodTaster;
  private final Serializer _snapshotSerializer;
  public StrictTransactionCensor(  GenericSnapshotManager snapshotManager){
    _king=snapshotManager.recoveredPrevalentSystem();
    _snapshotSerializer=snapshotManager.primarySerializer();
  }
  public void approve(  TransactionTimestamp transactionTimestamp) throws RuntimeException, Error {
    try {
      TransactionTimestamp timestampCopy=transactionTimestamp.cleanCopy();
      PrevalentSystemGuard royalFoodTaster=royalFoodTaster(transactionTimestamp.systemVersion() - 1);
      royalFoodTaster.receive(timestampCopy);
    }
 catch (    RuntimeException rx) {
      letTheFoodTasterDie();
      throw rx;
    }
catch (    Error error) {
      letTheFoodTasterDie();
      throw error;
    }
  }
  private void letTheFoodTasterDie(){
    _royalFoodTaster=null;
  }
  private PrevalentSystemGuard royalFoodTaster(  long systemVersion){
    if (_royalFoodTaster == null)     produceNewFoodTaster(systemVersion);
    return _royalFoodTaster;
  }
  private void produceNewFoodTaster(  long systemVersion){
    try {
      _royalFoodTaster=_king.deepCopy(systemVersion,_snapshotSerializer);
    }
 catch (    Exception ex) {
      ex.printStackTrace();
      throw new RuntimeException("Unable to produce a copy of the prevalent system for trying out transactions before applying them to the real system.");
    }
  }
}
