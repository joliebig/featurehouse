package org.prevayler.foundation;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileDescriptor;
import java.io.FileOutputStream;
import java.io.IOException;

public class DurableOutputStream {
  /** 
 * These two locks allow the two main activities of this class,
 * serializing transactions to a buffer on the one hand and flushing
 * the buffer and syncing to disk on the other hand, to proceed
 * concurrently. Note that where both locks are required, we always
 * acquire the _syncLock before acquiring the _writeLock to avoid
 * deadlock.
 */
  private final Object _writeLock=new Object();
  private final Object _syncLock=new Object();
  /** 
 * The File object is only stashed for the sake of the file() getter. 
 */
  private final File _file;
  /** 
 * All access guarded by _syncLock. 
 */
  private final FileOutputStream _fileOutputStream;
  /** 
 * All access guarded by _syncLock. 
 */
  private final FileDescriptor _fileDescriptor;
  /** 
 * All access guarded by _writeLock. 
 */
  private ByteArrayOutputStream _active=new ByteArrayOutputStream();
  /** 
 * All access guarded by _syncLock. 
 */
  private ByteArrayOutputStream _inactive=new ByteArrayOutputStream();
  /** 
 * All access guarded by _writeLock. 
 */
  private boolean _closed=false;
  /** 
 * All access guarded by _writeLock. 
 */
  private int _objectsWritten=0;
  /** 
 * All access guarded by _syncLock. 
 */
  private int _objectsSynced=0;
  /** 
 * All access guarded by _syncLock. 
 */
  private int _fileSyncCount=0;
  public DurableOutputStream(  File file) throws IOException {
    _file=file;
    _fileOutputStream=new FileOutputStream(file);
    _fileDescriptor=_fileOutputStream.getFD();
  }
  public void sync(  Guided guide) throws IOException {
    int thisWrite;
    guide.startTurn();
    try {
      thisWrite=writeObject(guide);
    }
  finally {
      guide.endTurn();
    }
    waitUntilSynced(thisWrite);
  }
  private int writeObject(  Guided guide) throws IOException {
synchronized (_writeLock) {
      if (_closed) {
        throw new IOException("already closed");
      }
      try {
        guide.writeTo(_active);
      }
 catch (      IOException exception) {
        internalClose();
        throw exception;
      }
      _objectsWritten++;
      return _objectsWritten;
    }
  }
  private void waitUntilSynced(  int thisWrite) throws IOException {
synchronized (_syncLock) {
      if (_objectsSynced < thisWrite) {
        int objectsWritten;
synchronized (_writeLock) {
          if (_closed) {
            throw new IOException("already closed");
          }
          ByteArrayOutputStream swap=_active;
          _active=_inactive;
          _inactive=swap;
          objectsWritten=_objectsWritten;
        }
        try {
          _inactive.writeTo(_fileOutputStream);
          _inactive.reset();
          _fileOutputStream.flush();
          _fileDescriptor.sync();
        }
 catch (        IOException exception) {
          internalClose();
          throw exception;
        }
        _objectsSynced=objectsWritten;
        _fileSyncCount++;
      }
    }
  }
  public void close() throws IOException {
synchronized (_syncLock) {
synchronized (_writeLock) {
        if (_closed) {
          return;
        }
        internalClose();
        _fileOutputStream.close();
      }
    }
  }
  private void internalClose(){
synchronized (_writeLock) {
      _closed=true;
      _active=null;
      _inactive=null;
    }
  }
  public File file(){
    return _file;
  }
  public synchronized int fileSyncCount(){
synchronized (_syncLock) {
      return _fileSyncCount;
    }
  }
  public boolean reallyClosed(){
synchronized (_writeLock) {
      return _closed;
    }
  }
}
