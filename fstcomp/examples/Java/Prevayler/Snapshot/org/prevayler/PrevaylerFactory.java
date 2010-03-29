package org.prevayler;
import org.prevayler.implementation.snapshot.GenericSnapshotManager;
import org.prevayler.implementation.snapshot.NullSnapshotManager;
public class PrevaylerFactory {
  private boolean _transientMode;
  private NullSnapshotManager _nullSnapshotManager;
  /** 
 * Determines whether the Prevayler created by this factory should be
 * transient (transientMode = true) or persistent (transientMode = false).
 * Default is persistent. A transient Prevayler will execute its
 * Transactions WITHOUT writing them to disk. This is useful for stand-alone
 * applications which have a "Save" button, for example, or for running
 * automated tests MUCH faster than with a persistent Prevayler.
 */
  public void configureTransientMode(  boolean transientMode){
    _transientMode=transientMode;
  }
  private void configureNullSnapshotManager(  NullSnapshotManager snapshotManager){
    _nullSnapshotManager=snapshotManager;
  }
  public void configureSnapshotSerializer(  JavaSerializer serializer){
    configureSnapshotSerializer("snapshot",serializer);
  }
  public void configureSnapshotSerializer(  XStreamSerializer serializer){
    configureSnapshotSerializer("xstreamsnapshot",serializer);
  }
  public void configureSnapshotSerializer(  SkaringaSerializer serializer){
    configureSnapshotSerializer("skaringasnapshot",serializer);
  }
  /** 
 * Configure a serialization strategy for snapshots. This may be called any
 * number of times with different suffixes to configure different strategies
 * for reading existing snapshots. The first call to this method establishes
 * the <i>primary</i> strategy, which will be used for writing snapshots as
 * well as for deep-copying the prevalent system whenever necessary.
 */
  public void configureSnapshotSerializer(  String suffix,  Serializer serializer){
    PrevaylerDirectory.checkValidSnapshotSuffix(suffix);
    _snapshotSerializers.put(suffix,serializer);
    if (_primarySnapshotSuffix == null) {
      _primarySnapshotSuffix=suffix;
    }
  }
  private TransactionPublisher publisher(  GenericSnapshotManager snapshotManager) throws IOException {
    try {
      if (_remoteServerIpAddress != null)       throw new ReturnObject(new ClientPublisher(network(),_remoteServerIpAddress,_remoteServerPort));
      if (true)       throw new ReturnObject(new CentralPublisher(clock(),censor(snapshotManager),journal()));
 else       throw new ReturnObject(new CentralPublisher(clock(),journal()));
      throw ReturnHack.returnObject;
    }
 catch (    ReturnObject r) {
      return (TransactionPublisher)r.value;
    }
  }
  private GenericSnapshotManager snapshotManager() throws ClassNotFoundException, IOException {
    if (_nullSnapshotManager != null)     return _nullSnapshotManager;
    PrevaylerDirectory directory=new PrevaylerDirectory(prevalenceDirectory());
    if (!_snapshotSerializers.isEmpty())     return new GenericSnapshotManager(_snapshotSerializers,_primarySnapshotSuffix,prevalentSystem(),directory,journalSerializer());
    String snapshotSuffix="snapshot";
    JavaSerializer snapshotSerializer=new JavaSerializer();
    return new GenericSnapshotManager(Collections.singletonMap(snapshotSuffix,snapshotSerializer),snapshotSuffix,prevalentSystem(),directory,journalSerializer());
  }
  protected static void hook69(  Serializable newPrevalentSystem,  PrevaylerFactory factory){
    factory.configureNullSnapshotManager(new NullSnapshotManager(newPrevalentSystem,"Transient Prevaylers are unable to take snapshots."));
    factory.configureTransientMode(true);
    original(newPrevalentSystem,factory);
  }
  protected static void hook70(  PrevaylerFactory factory){
    factory.configureTransientMode(true);
    original(factory);
  }
@MethodObject static class PrevaylerFactory_create {
    protected void hook71() throws IOException, ClassNotFoundException {
      if (true)       throw new ReturnObject(new PrevaylerImpl(snapshotManager,publisher,_this.journalSerializer()));
 else       throw new ReturnObject(new PrevaylerImpl(publisher,_this.journalSerializer()));
      original();
    }
    protected void hook72() throws IOException, ClassNotFoundException {
      if (true)       publisher=_this.publisher(snapshotManager);
 else       publisher=_this.publisher();
      original();
    }
    protected void hook73() throws IOException, ClassNotFoundException {
      snapshotManager=_this.snapshotManager();
      original();
    }
  }
@MethodObject static class PrevaylerFactory_journal {
    protected void hook74() throws IOException {
      if (_this._transientMode) {
        original();
      }
 else {
        directory=new PrevaylerDirectory(_this.prevalenceDirectory());
        if (true)         throw new ReturnObject(new PersistentJournal(directory,_this._journalSizeThreshold,_this._journalAgeThreshold,_this.journalSuffix(),_this.monitor()));
 else         throw new ReturnObject(new PersistentJournal(directory,_this._journalSizeThreshold,_this._journalAgeThreshold,_this.journalSuffix()));
      }
    }
  }
}
