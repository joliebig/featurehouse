package org.prevayler.foundation;
import java.io.BufferedInputStream;
import java.io.EOFException;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectStreamException;
import java.io.UTFDataFormatException;

public class DurableInputStream {
  private final File _file;
  private boolean _EOF=false;
  private InputStream _fileStream;
  public DurableInputStream(  File file) throws IOException {
    _file=file;
    _fileStream=new BufferedInputStream(new FileInputStream(file));
  }
  public Chunk readChunk() throws IOException {
    if (_EOF)     throw new EOFException();
    try {
      Chunk chunk=Chunking.readChunk(_fileStream);
      if (chunk != null)       return chunk;
    }
 catch (    EOFException eofx) {
    }
catch (    ObjectStreamException scx) {
      ignoreStreamCorruption(scx);
    }
catch (    UTFDataFormatException utfx) {
      ignoreStreamCorruption(utfx);
    }
catch (    RuntimeException rx) {
      ignoreStreamCorruption(rx);
    }
    _fileStream.close();
    _EOF=true;
    throw new EOFException();
  }
  private void ignoreStreamCorruption(  Exception ex){
    String message="Stream corruption found while reading a transaction from the journal. If this is a transaction that was being written when a system crash occurred, there is no problem because it was never executed on the Prevalent System. Before executing each transaction, Prevayler writes it to the journal and calls the java.io.FileDescritor.sync() method to instruct the Java API to physically sync all operating system RAM buffers to disk.";
    this.hook75(ex,message);
  }
  public void close() throws IOException {
    _fileStream.close();
    _EOF=true;
  }
  protected void hook75(  Exception ex,  String message){
  }
}
