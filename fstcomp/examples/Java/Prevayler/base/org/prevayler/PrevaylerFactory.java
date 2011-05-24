package org.prevayler;
import org.prevayler.foundation.serialization.JavaSerializer;
import org.prevayler.foundation.serialization.Serializer;
import org.prevayler.foundation.serialization.SkaringaSerializer;
import org.prevayler.foundation.serialization.XStreamSerializer;
import org.prevayler.implementation.PrevaylerDirectory;
import org.prevayler.implementation.PrevaylerImpl;
import org.prevayler.implementation.clock.MachineClock;
import org.prevayler.implementation.journal.Journal;
import org.prevayler.implementation.journal.PersistentJournal;
import org.prevayler.implementation.journal.TransientJournal;
import org.prevayler.implementation.publishing.CentralPublisher;
import org.prevayler.implementation.publishing.TransactionPublisher;
import java.io.IOException;
import java.io.Serializable;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

/** 
 * Provides easy access to all Prevayler configurations and implementations
 * available in this distribution. Static methods are also provided as
 * short-cuts for the most common configurations. <br>
 * By default, the Prevayler instances created by this class will write their
 * Transactions to .journal files before executing them. The
 * FileDescriptor.sync() method is called to make sure the Java file
 * write-buffers have been written to the operating system. Many operating
 * systems, including most recent versions of Linux and Windows, allow the
 * hard-drive's write-cache to be disabled. This guarantees no executed
 * Transaction will be lost in the event of a power shortage, for example. <br>
 * Also by default, the Prevayler instances created by this class will filter
 * out all Transactions that would throw a RuntimeException or Error if executed
 * on the Prevalent System. This requires enough RAM to hold another copy of the
 * prevalent system.
 * @see Prevayler
 */
public class PrevaylerFactory {
  private Object _prevalentSystem;
  private Clock _clock;
  private boolean _transactionFiltering=true;
  private String _prevalenceDirectory;
  private long _journalSizeThreshold;
  private long _journalAgeThreshold;
  private Serializer _journalSerializer;
  private String _journalSuffix;
  private Map _snapshotSerializers=new HashMap();
  private String _primarySnapshotSuffix;
  /** 
 * Creates a Prevayler that will use a directory called "PrevalenceBase"
 * under the current directory to read and write its .snapshot and .journal
 * files.
 * @param newPrevalentSystemThe newly started, "empty" prevalent system that will be used
 * as a starting point for every system startup, until the first
 * snapshot is taken.
 */
  public static Prevayler createPrevayler(  Serializable newPrevalentSystem) throws IOException, ClassNotFoundException {
    return createPrevayler(newPrevalentSystem,"PrevalenceBase");
  }
  /** 
 * Creates a Prevayler that will use the given prevalenceBase directory to
 * read and write its .snapshot and .journal files.
 * @param newPrevalentSystemThe newly started, "empty" prevalent system that will be used
 * as a starting point for every system startup, until the first
 * snapshot is taken.
 * @param prevalenceBaseThe directory where the .snapshot files and .journal files
 * will be read and written.
 */
  public static Prevayler createPrevayler(  Serializable newPrevalentSystem,  String prevalenceBase) throws IOException, ClassNotFoundException {
    PrevaylerFactory factory=new PrevaylerFactory();
    factory.configurePrevalentSystem(newPrevalentSystem);
    factory.configurePrevalenceDirectory(prevalenceBase);
    return factory.create();
  }
  /** 
 * Creates a Prevayler that will execute Transactions WITHOUT writing them
 * to disk. This is useful for running automated tests or demos MUCH faster
 * than with a regular Prevayler.
 * Attempts to take snapshots on this transient Prevayler will throw an
 * IOException.
 * @param newPrevalentSystemThe newly started, "empty" prevalent system.
 * @see #createCheckpointPrevayler(Serializable newPrevalentSystem,String snapshotDirectory)
 */
  public static Prevayler createTransientPrevayler(  Serializable newPrevalentSystem){
    PrevaylerFactory factory=new PrevaylerFactory();
    factory.configurePrevalentSystem(newPrevalentSystem);
    hook69(newPrevalentSystem,factory);
    try {
      return factory.create();
    }
 catch (    Exception e) {
      e.printStackTrace();
      return null;
    }
  }
  /** 
 * @deprecated Use createCheckpointPrevayler() instead of this method.
 * Deprecated since Prevayler2.00.001.
 */
  public static Prevayler createTransientPrevayler(  Serializable newPrevalentSystem,  String snapshotDirectory){
    return createCheckpointPrevayler(newPrevalentSystem,snapshotDirectory);
  }
  /** 
 * Creates a Prevayler that will execute Transactions WITHOUT writing them
 * to disk. Snapshots will work as "checkpoints" for the system, therefore.
 * This is useful for stand-alone applications that have a "Save" button,
 * for example.
 * @param newPrevalentSystemThe newly started, "empty" prevalent system that will be used
 * as a starting point for every system startup, until the first
 * snapshot is taken.
 * @param snapshotDirectoryThe directory where the .snapshot files will be read and
 * written.
 */
  public static Prevayler createCheckpointPrevayler(  Serializable newPrevalentSystem,  String snapshotDirectory){
    PrevaylerFactory factory=new PrevaylerFactory();
    factory.configurePrevalentSystem(newPrevalentSystem);
    factory.configurePrevalenceDirectory(snapshotDirectory);
    hook70(factory);
    try {
      return factory.create();
    }
 catch (    Exception e) {
      e.printStackTrace();
      return null;
    }
  }
  private Clock clock(){
    return _clock != null ? _clock : new MachineClock();
  }
  /** 
 * Configures the Clock that will be used by the created Prevayler. The
 * Clock interface can be implemented by the application if it requires
 * Prevayler to use a special time source other than the machine clock
 * (default).
 */
  public void configureClock(  Clock clock){
    _clock=clock;
  }
  /** 
 * Configures the directory where the created Prevayler will read and write
 * its .journal and .snapshot files. The default is a directory called
 * "PrevalenceBase" under the current directory.
 * @param prevalenceDirectoryWill be ignored for the .snapshot files if a SnapshotManager
 * is configured.
 */
  public void configurePrevalenceDirectory(  String prevalenceDirectory){
    _prevalenceDirectory=prevalenceDirectory;
  }
  /** 
 * Configures the prevalent system that will be used by the Prevayler
 * created by this factory.
 * @param newPrevalentSystemIf the default Serializer is used, this prevalentSystem must
 * be Serializable. If another Serializer is used, this
 * prevalentSystem must be compatible with it.
 * @see #configureSnapshotSerializer(String,Serializer)
 */
  public void configurePrevalentSystem(  Object newPrevalentSystem){
    _prevalentSystem=newPrevalentSystem;
  }
  /** 
 * Determines whether the Prevayler created by this factory should filter
 * out all Transactions that would throw a RuntimeException or Error if
 * executed on the Prevalent System (default is true). This requires enough
 * RAM to hold another copy of the prevalent system.
 */
  public void configureTransactionFiltering(  boolean transactionFiltering){
    _transactionFiltering=transactionFiltering;
  }
  /** 
 * Configures the size (in bytes) of the journal file. When the current
 * journal exceeds this size, a new journal is created.
 */
  public void configureJournalFileSizeThreshold(  long sizeInBytes){
    _journalSizeThreshold=sizeInBytes;
  }
  /** 
 * Sets the age (in milliseconds) of the journal file. When the current
 * journal expires, a new journal is created.
 */
  public void configureJournalFileAgeThreshold(  long ageInMilliseconds){
    _journalAgeThreshold=ageInMilliseconds;
  }
  public void configureJournalSerializer(  JavaSerializer serializer){
    configureJournalSerializer("journal",serializer);
  }
  public void configureJournalSerializer(  XStreamSerializer serializer){
    configureJournalSerializer("xstreamjournal",serializer);
  }
  public void configureJournalSerializer(  SkaringaSerializer serializer){
    configureJournalSerializer("skaringajournal",serializer);
  }
  /** 
 * Configures the transaction journal Serializer to be used by the Prevayler
 * created by this factory. Only one Serializer is supported at a time. If
 * you want to change the Serializer of a system in production, you will
 * have to take a snapshot first because the journal files written by the
 * previous Serializer will not be read.
 */
  public void configureJournalSerializer(  String suffix,  Serializer serializer){
    PrevaylerDirectory.checkValidJournalSuffix(suffix);
    if (_journalSerializer != null) {
      throw new IllegalStateException("Read the javadoc to this method.");
    }
    _journalSerializer=serializer;
    _journalSuffix=suffix;
  }
  /** 
 * Returns a Prevayler created according to what was defined by calls to the
 * configuration methods above.
 * @throws IOExceptionIf there is trouble creating the Prevalence Base directory or
 * reading a .journal or .snapshot file.
 * @throws ClassNotFoundExceptionIf a class of a serialized Object is not found when reading a
 * .journal or .snapshot file.
 */
  public Prevayler create() throws IOException, ClassNotFoundException {
    return new PrevaylerFactory_create(this).execute();
  }
  private String prevalenceDirectory(){
    return _prevalenceDirectory != null ? _prevalenceDirectory : "Prevalence";
  }
  private Object prevalentSystem(){
    if (_prevalentSystem == null)     throw new IllegalStateException("The prevalent system must be configured.");
    return _prevalentSystem;
  }
  private TransactionPublisher publisher() throws IOException {
    try {
      this.hook67();
      return new CentralPublisher(clock(),journal());
    }
 catch (    ReturnObject r) {
      return (TransactionPublisher)r.value;
    }
  }
  private Journal journal() throws IOException {
    return new PrevaylerFactory_journal(this).execute();
  }
  private Serializer journalSerializer(){
    if (_journalSerializer != null)     return _journalSerializer;
    return new JavaSerializer();
  }
  private String journalSuffix(){
    return _journalSuffix != null ? _journalSuffix : "journal";
  }
@MethodObject static class PrevaylerFactory_create {
    PrevaylerFactory_create(    PrevaylerFactory _this){
      this._this=_this;
    }
    Prevayler execute() throws IOException, ClassNotFoundException {
      try {
        this.hook73();
{
        }
        this.hook72();
        this.hook68();
        this.hook71();
        throw ReturnHack.returnObject;
      }
 catch (      ReturnObject r) {
        return (Prevayler)r.value;
      }
    }
    protected PrevaylerFactory _this;
    protected GenericSnapshotManager snapshotManager;
    protected TransactionPublisher publisher;
    protected void hook68() throws IOException, ClassNotFoundException {
    }
    protected void hook71() throws IOException, ClassNotFoundException {
    }
    protected void hook72() throws IOException, ClassNotFoundException {
    }
    protected void hook73() throws IOException, ClassNotFoundException {
    }
  }
@MethodObject static class PrevaylerFactory_journal {
    PrevaylerFactory_journal(    PrevaylerFactory _this){
      this._this=_this;
    }
    Journal execute() throws IOException {
      try {
        this.hook74();
        throw ReturnHack.returnObject;
      }
 catch (      ReturnObject r) {
        return (Journal)r.value;
      }
    }
    protected PrevaylerFactory _this;
    protected PrevaylerDirectory directory;
    protected void hook74() throws IOException {
      throw new ReturnObject((Journal)new TransientJournal());
    }
  }
  protected void hook67() throws IOException {
  }
  protected static void hook69(  Serializable newPrevalentSystem,  PrevaylerFactory factory){
  }
  protected static void hook70(  PrevaylerFactory factory){
  }
}
