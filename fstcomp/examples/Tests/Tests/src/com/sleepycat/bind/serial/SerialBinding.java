package com.sleepycat.bind.serial; 
import java.io.IOException; 
import com.sleepycat.bind.EntryBinding; 
import com.sleepycat.je.DatabaseEntry; 
import com.sleepycat.util.FastInputStream; 
import com.sleepycat.util.FastOutputStream; 
import com.sleepycat.util.RuntimeExceptionWrapper; 
import de.ovgu.cide.jakutil.*; 
public  class  SerialBinding  extends SerialBase  implements EntryBinding {
	 private ClassCatalog classCatalog;

	 private Class baseClass;

	 public SerialBinding( ClassCatalog classCatalog, Class baseClass){ if (classCatalog == null) { throw new NullPointerException("classCatalog must be non-null"); } this.classCatalog=classCatalog; this.baseClass=baseClass; }

	 public final Class getBaseClass(){ return baseClass; }

	 public ClassLoader getClassLoader(){ return null; }

	 public Object entryToObject( DatabaseEntry entry){ int length=entry.getSize(); byte[] hdr=SerialOutput.getStreamHeader(); byte[] bufWithHeader=new byte[length + hdr.length]; System.arraycopy(hdr,0,bufWithHeader,0,hdr.length); System.arraycopy(entry.getData(),entry.getOffset(),bufWithHeader,hdr.length,length); try { SerialInput jin=new SerialInput(new FastInputStream(bufWithHeader,0,bufWithHeader.length),classCatalog,getClassLoader()); return jin.readObject(); } catch ( IOException e) { throw new RuntimeExceptionWrapper(e); }
catch ( ClassNotFoundException e) { throw new RuntimeExceptionWrapper(e); } }

	 public void objectToEntry( Object object, DatabaseEntry entry){ if (baseClass != null && !baseClass.isInstance(object)) { throw new IllegalArgumentException("Data object class (" + object.getClass() + ") not an instance of binding's base class ("+ baseClass+ ')'); } FastOutputStream fo=getSerialOutput(object); try { SerialOutput jos=new SerialOutput(fo,classCatalog); jos.writeObject(object); } catch ( IOException e) { throw new RuntimeExceptionWrapper(e); } byte[] hdr=SerialOutput.getStreamHeader(); entry.setData(fo.getBufferBytes(),hdr.length,fo.getBufferLength() - hdr.length); }


}
