package net.sf.jabref.collab;

import net.sf.jabref.Globals;
import net.sf.jabref.Util;
import java.util.HashMap;
import java.io.File;
import java.io.IOException;
import java.util.Iterator;


public class FileUpdateMonitor extends Thread {

  final int WAIT = 4000;
  static int tmpNum = 0;
  int no = 0;
  HashMap<String, Entry> entries = new HashMap<String, Entry>();
  boolean running;

  public FileUpdateMonitor() {
    setPriority(MIN_PRIORITY);
  }

  public void run() {
    running = true;

    
    while (running) {
      
      Iterator<String> i = entries.keySet().iterator();
      for (;i.hasNext();) {
        Entry e = entries.get(i.next());
        try {
          if (e.hasBeenUpdated())
            e.notifyListener();

          
          
        } catch (IOException ex) {
          e.notifyFileRemoved();
        }
      }

      
      try {
        sleep(WAIT);
      } catch (InterruptedException ex) {
      }
    }
  }

  
  public void stopMonitoring() {
    running = false;
  }

  
  public String addUpdateListener(FileUpdateListener ul, File file) throws IOException {
     
    if (!file.exists())
      throw new IOException("File not found");
    no++;
    String key = ""+no;
    entries.put(key, new Entry(ul, file));
    return key;
  }

    
    public boolean hasBeenModified(String handle) throws IllegalArgumentException {
	Object o = entries.get(handle);
	if (o == null)
            return false;
        
	try {
	    return ((Entry)o).hasBeenUpdated();
	} catch (IOException ex) {
	    
	    return false;
	}
    }

    
    public void perturbTimestamp(String handle) {
        Object o = entries.get(handle);
        if (o == null)
            return;
        ((Entry)o).timeStamp--;
    }

  
  public void removeUpdateListener(String handle) {
    entries.remove(handle);
  }

  public void updateTimeStamp(String key) throws IllegalArgumentException {
    Object o = entries.get(key);
    if (o == null)
      throw new IllegalArgumentException("Entry not found");
    Entry entry = (Entry)o;
    entry.updateTimeStamp();

  }

  public void changeFile(String key, File file) throws IOException, IllegalArgumentException {
    if (!file.exists())
      throw new IOException("File not found");
    Object o = entries.get(key);
    if (o == null)
      throw new IllegalArgumentException("Entry not found");
    ((Entry)o).file = file;
  }

  
  public File getTempFile(String key) throws IllegalArgumentException {
    Object o = entries.get(key);
    if (o == null)
      throw new IllegalArgumentException("Entry not found");
    return ((Entry)o).tmpFile;
  }

  
  class Entry {
    FileUpdateListener listener;
    File file;
    File tmpFile;
    long timeStamp;

    public Entry(FileUpdateListener ul, File f) {
      listener = ul;
      file = f;
      timeStamp = file.lastModified();
      tmpFile = getTempFile();
      copy();
    }

    
    public boolean hasBeenUpdated() throws IOException {
      long modified = file.lastModified();
      if (modified == 0L)
        throw new IOException("File deleted");
      return timeStamp != modified;
    }

    public void updateTimeStamp() {
      timeStamp = file.lastModified();
      if (timeStamp == 0L)
        notifyFileRemoved();

      copy();
    }

    public boolean copy() {
	
	
      boolean res = false;
      try {
        res = Util.copyFile(file, tmpFile, true);
      } catch (IOException ex) {
        Globals.logger("Cannot copy to temporary file '"+tmpFile.getPath()+"'");
      }
      
      return res;
	
      
    }

    
    public void notifyListener() {
      
      timeStamp = file.lastModified();
      listener.fileUpdated();
    }

    
    public void notifyFileRemoved() {
      listener.fileRemoved();
    }

    public void finalize() {
      try {
        tmpFile.delete();
      } catch (Throwable e) {
        Globals.logger("Cannot delete temporary file '"+tmpFile.getPath()+"'");
      }
    }
  }

  static synchronized File getTempFile() {
    File f = null;
    
    
    try {
	    f = File.createTempFile("jabref", null);
        f.deleteOnExit();
	
    } catch (IOException ex) {
	ex.printStackTrace();
    }
    return f;
  }
}
