package org.prevayler.implementation.journal;
import org.prevayler.foundation.monitor.Monitor;
public class PersistentJournal {
  private Monitor _monitor;
  /** 
 * @param directory
 * @param journalSizeThresholdInBytesSize of the current journal file beyond which it is closed and
 * a new one started. Zero indicates no size threshold. This is
 * useful journal backup purposes.
 * @param journalAgeThresholdInMillisAge of the current journal file beyond which it is closed and
 * a new one started. Zero indicates no age threshold. This is
 * useful journal backup purposes.
 */
  PersistentJournal(  PrevaylerDirectory directory,  long journalSizeThresholdInBytes,  long journalAgeThresholdInMillis,  String journalSuffix,  Monitor monitor) throws IOException {
    this(directory,journalSizeThresholdInBytes,journalAgeThresholdInMillis,journalSuffix);
    _monitor=monitor;
  }
  protected DurableInputStream hook76(  File journal,  DurableInputStream input) throws IOException {
    if (true)     input=new DurableInputStream(journal,_monitor);
 else     input=new DurableInputStream(journal);
    return original(journal,input);
  }
  protected void hook77(  File journal,  DurableInputStream input) throws IOException {
    if (true)     input=new DurableInputStream(journal,_monitor);
 else     input=new DurableInputStream(journal);
    original(journal,input);
  }
  protected void hook78(  IOException iox,  File journal,  String message){
    _monitor.notify(this.getClass(),message,journal,iox);
    original(iox,journal,message);
  }
}
