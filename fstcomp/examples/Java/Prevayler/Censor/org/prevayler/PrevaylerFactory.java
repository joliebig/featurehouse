package org.prevayler;
import org.prevayler.implementation.publishing.censorship.LiberalTransactionCensor;
import org.prevayler.implementation.publishing.censorship.StrictTransactionCensor;
import org.prevayler.implementation.publishing.censorship.TransactionCensor;
public class PrevaylerFactory {
  private TransactionCensor censor(  GenericSnapshotManager snapshotManager){
    return _transactionFiltering ? (TransactionCensor)new StrictTransactionCensor(snapshotManager) : new LiberalTransactionCensor();
  }
}
