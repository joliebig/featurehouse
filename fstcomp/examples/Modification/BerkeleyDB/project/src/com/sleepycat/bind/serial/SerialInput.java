package com.sleepycat.bind.serial; 
import java.io.IOException; 
import java.io.InputStream; 
import java.io.ObjectInputStream; 
import java.io.ObjectStreamClass; 
import com.sleepycat.je.DatabaseException; 
import com.sleepycat.util.RuntimeExceptionWrapper; 
import de.ovgu.cide.jakutil.*; 
public  class  SerialInput  extends ObjectInputStream {
	 private ClassCatalog classCatalog;

	 private ClassLoader classLoader;

	 public SerialInput( InputStream in, ClassCatalog classCatalog) throws IOException { this(in,classCatalog,null); }

	 public SerialInput( InputStream in, ClassCatalog classCatalog, ClassLoader classLoader) throws IOException { super(in); this.classCatalog=classCatalog; this.classLoader=classLoader; }

	 protected ObjectStreamClass readClassDescriptor__wrappee__base() throws IOException, ClassNotFoundException { try { byte len=readByte(); byte[] id=new byte[len]; readFully(id); return classCatalog.getClassFormat(id); } catch ( DatabaseException e) { throw new RuntimeExceptionWrapper(e); } }

	 protected ObjectStreamClass readClassDescriptor() throws IOException, ClassNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	readClassDescriptor__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	 protected Class resolveClass__wrappee__base( ObjectStreamClass desc) throws IOException, ClassNotFoundException { if (classLoader != null) { try { return Class.forName(desc.getName(),false,classLoader); } catch ( ClassNotFoundException e) { return super.resolveClass(desc); } } else { return super.resolveClass(desc); } }

	 protected Class resolveClass( ObjectStreamClass desc) throws IOException, ClassNotFoundException { t.in(Thread.currentThread().getStackTrace()[1].toString());	resolveClass__wrappee__base(); t.out(Thread.currentThread().getStackTrace()[1].toString()); }

	private Tracer t = new Tracer();

	public Tracer getTracer(){return t;}


}
