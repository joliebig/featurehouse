package com.sleepycat.bind.serial; 
import java.io.ByteArrayOutputStream; 
import java.io.IOException; 
import java.io.ObjectOutputStream; 
import java.io.ObjectStreamClass; 
import java.io.ObjectStreamConstants; 
import java.io.OutputStream; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.util.RuntimeExceptionWrapper; 
import de.ovgu.cide.jakutil.*; 
public  class  SerialOutput  extends ObjectOutputStream {
	 private final static byte[] STREAM_HEADER;

	
static { ByteArrayOutputStream baos=new ByteArrayOutputStream(); try { new SerialOutput(baos,null); } catch ( IOException e) { throw new RuntimeExceptionWrapper(e); } STREAM_HEADER=baos.toByteArray(); }

	 private ClassCatalog classCatalog;

	 public SerialOutput( OutputStream out, ClassCatalog classCatalog) throws IOException { super(out); this.classCatalog=classCatalog; useProtocolVersion(ObjectStreamConstants.PROTOCOL_VERSION_2); }

	 protected void writeClassDescriptor( ObjectStreamClass classdesc) throws IOException { try { byte[] id=classCatalog.getClassID(classdesc); writeByte(id.length); write(id); } catch ( DatabaseException e) { throw new RuntimeExceptionWrapper(e); }
catch ( ClassNotFoundException e) { throw new RuntimeExceptionWrapper(e); } }

	 public static byte[] getStreamHeader(){ return STREAM_HEADER; }


}
