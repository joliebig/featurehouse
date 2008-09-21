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

	 public final Class getBaseClass__wrappee__base(){ return baseClass; }

	 public final Class getBaseClass(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getBaseClass__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public ClassLoader getClassLoader__wrappee__base(){ return null; }

	 public ClassLoader getClassLoader(){ t.in(Thread.currentThread().getStackTrace()[1].toString());	getClassLoader__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public Object entryToObject__wrappee__base( DatabaseEntry entry){ int length=entry.getSize(); byte[] hdr=SerialOutput.getStreamHeader(); byte[] bufWithHeader=new byte[length + hdr.length]; System.arraycopy(hdr,0,bufWithHeader,0,hdr.length); System.arraycopy(entry.getData(),entry.getOffset(),bufWithHeader,hdr.length,length); try { SerialInput jin=new SerialInput(new FastInputStream(bufWithHeader,0,bufWithHeader.length),classCatalog,getClassLoader()); return jin.readObject(); } catch ( IOException e) { throw new RuntimeExceptionWrapper(e); }
catch ( ClassNotFoundException e) { throw new RuntimeExceptionWrapper(e); } }

	 public Object entryToObject( DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	entryToObject__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 public void objectToEntry__wrappee__base( Object object, DatabaseEntry entry){ if (baseClass != null && !baseClass.isInstance(object)) { throw new IllegalArgumentException("Data object class (" + object.getClass() + ") not an instance of binding's base class ("+ baseClass+ ')'); } FastOutputStream fo=getSerialOutput(object); try { SerialOutput jos=new SerialOutput(fo,classCatalog); jos.writeObject(object); } catch ( IOException e) { throw new RuntimeExceptionWrapper(e); } byte[] hdr=SerialOutput.getStreamHeader(); entry.setData(fo.getBufferBytes(),hdr.length,fo.getBufferLength() - hdr.length); }

	 public void objectToEntry( Object object, DatabaseEntry entry){ t.in(Thread.currentThread().getStackTrace()[1].toString());	objectToEntry__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
