

package org.gjt.sp.jedit;


import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.gjt.sp.util.Log;



public class JARClassLoader extends ClassLoader
{
	
	
	public JARClassLoader()
	{
	} 

	
	
	public Class loadClass(String clazz, boolean resolveIt)
		throws ClassNotFoundException
	{
		
		Object obj = classHash.get(clazz);
		if(obj == NO_CLASS)
		{
			
			
			
			throw new ClassNotFoundException(clazz);
		}
		else if(obj instanceof JARClassLoader)
		{
			JARClassLoader classLoader = (JARClassLoader)obj;
			return classLoader._loadClass(clazz,resolveIt);
		}

		
		
		try
		{
			Class cls;

			
			ClassLoader parentLoader = getClass().getClassLoader();
			if (parentLoader != null)
				cls = parentLoader.loadClass(clazz);
			else
				cls = findSystemClass(clazz);

			return cls;
		}
		catch(ClassNotFoundException cnf)
		{
			
			
			classHash.put(clazz,NO_CLASS);

			throw cnf;
		}
	} 

	
	public InputStream getResourceAsStream(String name)
	{
		if(jar == null)
			return null;

		try
		{
			ZipFile zipFile = jar.getZipFile();
			ZipEntry entry = zipFile.getEntry(name);
			if(entry == null)
				return getSystemResourceAsStream(name);
			else
				return zipFile.getInputStream(entry);
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,this,io);

			return null;
		}
	} 

	
	public URL getResource(String name)
	{
		if(jar == null)
			return null;

		try
		{
			ZipFile zipFile = jar.getZipFile();
			ZipEntry entry = zipFile.getEntry(name);
			if(entry == null)
				return getSystemResource(name);
			else
				return new URL(getResourceAsPath(name));
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,this,io);
			return null;
		}
	} 

	
	public String getResourceAsPath(String name)
	{
		if(jar == null)
			return null;

		if(!name.startsWith("/"))
			name = "/" + name;

		return "jeditresource:/" + MiscUtilities.getFileName(
			jar.getPath()) + "!" + name;
	} 

	
	
	public ZipFile getZipFile()
	{
		try
		{
			return jar.getZipFile();
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,this,io);
			return null;
		}
	} 

	

	
	
	JARClassLoader(PluginJAR jar)
	{
		this.jar = jar;
	} 

	
	void activate()
	{
		String[] classes = jar.getClasses();
		if(classes != null)
		{
			for(int i = 0; i < classes.length; i++)
			{
				classHash.put(classes[i],this);
			}
		}
	} 

	
	void deactivate()
	{
		String[] classes = jar.getClasses();
		for(int i = 0; i < classes.length; i++)
		{
			Object loader = classHash.get(classes[i]);
			if(loader == this)
				classHash.remove(classes[i]);
			else
				;
		}
	} 

	

	

	
	private static final Object NO_CLASS = new Object();

	private static Hashtable classHash = new Hashtable();

	private PluginJAR jar;

	
	
	private Class _loadClass(String clazz, boolean resolveIt)
		throws ClassNotFoundException
	{
		jar.activatePlugin();

		Class cls = findLoadedClass(clazz);
		if(cls != null)
		{
			if(resolveIt)
				resolveClass(cls);
			return cls;
		}

		String name = MiscUtilities.classToFile(clazz);

		try
		{
			ZipFile zipFile = jar.getZipFile();
			ZipEntry entry = zipFile.getEntry(name);

			if(entry == null)
				throw new ClassNotFoundException(clazz);

			InputStream in = zipFile.getInputStream(entry);

			int len = (int)entry.getSize();
			byte[] data = new byte[len];
			int success = 0;
			int offset = 0;
			while(success < len)
			{
				len -= success;
				offset += success;
				success = in.read(data,offset,len);
				if(success == -1)
				{
					Log.log(Log.ERROR,this,"Failed to load class "
						+ clazz + " from " + zipFile.getName());
					throw new ClassNotFoundException(clazz);
				}
			}

			cls = defineClass(clazz,data,0,data.length);

			if(resolveIt)
				resolveClass(cls);

			return cls;
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,this,io);

			throw new ClassNotFoundException(clazz);
		}
	} 

	
}
