package org.prevayler.implementation;
import org.prevayler.implementation.snapshot.GenericSnapshotManager;
public class PrevaylerImpl {
  private PrevalentSystemGuard _guard;
  private GenericSnapshotManager _snapshotManager;
  /** 
 * Creates a new Prevayler
 * @param snapshotManagerThe SnapshotManager that will be used for reading and writing
 * snapshot files.
 * @param transactionPublisherThe TransactionPublisher that will be used for publishing
 * transactions executed with this PrevaylerImpl.
 * @param prevaylerMonitorThe Monitor that will be used to monitor interesting calls to
 * this PrevaylerImpl.
 * @param journalSerializer
 */
  PrevaylerImpl(  GenericSnapshotManager snapshotManager,  TransactionPublisher transactionPublisher,  Serializer journalSerializer) throws IOException, ClassNotFoundException {
    this(transactionPublisher,journalSerializer);
    _snapshotManager=snapshotManager;
    _guard=_snapshotManager.recoveredPrevalentSystem();
    _guard.subscribeTo(_publisher);
  }
  public Object prevalentSystem(){
    return _guard.prevalentSystem();
  }
  public Object execute(  Query sensitiveQuery) throws Exception {
    return _guard.executeQuery(sensitiveQuery,clock());
  }
  public void takeSnapshot() throws IOException {
    _guard.takeSnapshot(_snapshotManager);
  }
}
