

package org.gjt.sp.jedit;


import java.io.InputStream;
import java.io.IOException;
import java.net.URL;
import java.util.*;
import java.util.zip.ZipEntry;
import java.util.zip.ZipFile;
import org.gjt.sp.util.Log;

import java.util.jar.Manifest;
import java.util.jar.JarFile;
import java.net.MalformedURLException;
import java.util.jar.Attributes;
import java.util.jar.Attributes.Name;



public class JARClassLoader extends ClassLoader
{
	
	
	public JARClassLoader()
	{
		this(true);
	}

	
	public JARClassLoader(boolean delegateFirst)
	{
		this.delegateFirst = delegateFirst;
		
		id = INDEX++;
		live++;
	} 

	
	
	public Class loadClass(String clazz, boolean resolveIt)
		throws ClassNotFoundException
	{
		ClassNotFoundException pending = null;
		if (delegateFirst)
		{
			try
			{
				return loadFromParent(clazz);
			}
			catch (ClassNotFoundException cnf)
			{
				
				pending = cnf;
			}
		}

		Object obj = classHash.get(clazz);
		if(obj == NO_CLASS)
		{
			
			
			
			throw new ClassNotFoundException(clazz);
		}
		else if(obj instanceof JARClassLoader)
		{
			JARClassLoader classLoader = (JARClassLoader)obj;
			try
			{
				return classLoader._loadClass(clazz,resolveIt);
			} catch (ClassNotFoundException cnf2)
			{
				classHash.put(clazz,NO_CLASS);
				throw cnf2;
			}
		}
		else if (delegateFirst)
		{
			
			
			
			throw pending;
		}

		return loadFromParent(clazz);
	} 

	
	public InputStream getResourceAsStream(String name)
	{
		try
		{
			
			if(jar != null)
			{
				ZipFile zipFile = jar.getZipFile();
				ZipEntry entry = zipFile.getEntry(name);
				if(entry != null)
				{
					return zipFile.getInputStream(entry);
				}
			}
			
			Object obj = resourcesHash.get(name);
			if(obj instanceof JARClassLoader)
			{
				JARClassLoader classLoader = (JARClassLoader)obj;
				return classLoader.getResourceAsStream(name);
			}
			
			return getSystemResourceAsStream(name);
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,this,io);

			return null;
		}
	} 

	
	
	public URL getResource(String name)
	{
		try
		{
			if(jar != null)
			{
				ZipFile zipFile = jar.getZipFile();
				ZipEntry entry = zipFile.getEntry(name);
				if(entry != null)
				{
					return new URL(getResourceAsPath(name));
				}
			}
			
			Object obj = resourcesHash.get(name);
			if(obj instanceof JARClassLoader)
			{
				JARClassLoader classLoader = (JARClassLoader)obj;
				return classLoader.getResource(name);
			} else
			{
				URL ret = getSystemResource(name); 
				if(ret != null)
				{
					Log.log(Log.DEBUG,JARClassLoader.class,"Would have returned null for getResource("+name+")");
					Log.log(Log.DEBUG,JARClassLoader.class,"returning("+ret+")");
				}
				return ret;
			}
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
			throw new UnsupportedOperationException(
				"don't call getResourceAsPath() on anonymous JARClassLoader");

		if(!name.startsWith("/"))
			name = '/' + name;

		return "jeditresource:/" + MiscUtilities.getFileName(
			jar.getPath()) + '!' + name;
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

	
	
	public static void dump()
	{
		Log.log(Log.DEBUG,JARClassLoader.class,
			"Total instances created: " + INDEX);
		Log.log(Log.DEBUG,JARClassLoader.class,
			"Live instances: " + live);
		synchronized(classHash)
		{
			for (Map.Entry<String, Object> entry : classHash.entrySet())
			{
				if (entry.getValue() != NO_CLASS)
				{
					Log.log(Log.DEBUG, JARClassLoader.class,
						entry.getKey() + " ==> "
							+ entry.getValue());
				}
			}
		}
	} 

	
	public String toString()
	{
		if(jar == null)
			return "<anonymous>(" + id + ')';
		else
			return jar.getPath() + " (" + id + ')';
	} 

	
	
	public Enumeration getResources(String name) throws IOException
	{
		class SingleElementEnumeration implements Enumeration
		{
			private Object element;

			SingleElementEnumeration(Object element)
			{
				this.element = element;
			}

			public boolean hasMoreElements()
			{
				return element != null;
			}

			public Object nextElement()
			{
				if(element != null)
				{
					Object retval = element;
					element = null;
					return retval;
				}
				else
					throw new NoSuchElementException();
			}
		}

		URL resource = getResource(name);
		return new SingleElementEnumeration(resource);
	} 

	
	protected void finalize()
	{
		live--;
	} 

	

	
	
	JARClassLoader(PluginJAR jar)
	{
		this();
		this.jar = jar;
	} 

	
	void activate()
	{
		if (jar.getPlugin() != null)
		{
			String _delegate = jEdit.getProperty(
				"plugin." + jar.getPlugin().getClassName() + ".class_loader_delegate");
			delegateFirst = _delegate == null || "true".equals(_delegate);
		}

		String[] classes = jar.getClasses();
		if(classes != null)
		{
			for(int i = 0; i < classes.length; i++)
			{
				classHash.put(classes[i],this);
			}
		}

		String[] resources = jar.getResources();
		if(resources != null)
		{
			for(int i = 0; i < resources.length; i++)
			{
				resourcesHash.put(resources[i],this);
			}
		}
	} 

	
	void deactivate()
	{
		String[] classes = jar.getClasses();
		if(classes != null)
		{
			for(int i = 0; i < classes.length; i++)
			{
				Object loader = classHash.get(classes[i]);
				if(loader == this)
					classHash.remove(classes[i]);
				else
					;
			}
		}

		String[] resources = jar.getResources();
		if(resources == null)
			return;

		for(int i = 0; i < resources.length; i++)
		{
			Object loader = resourcesHash.get(resources[i]);
			if(loader == this)
				resourcesHash.remove(resources[i]);
			else
				;
		}
	} 

	

	

	
	private static final Object NO_CLASS = new Object();

	private static int INDEX;
	private static int live;
	private static Map<String, Object> classHash = new Hashtable<String, Object>();
	private static Map<String, Object> resourcesHash = new HashMap<String, Object>();

	private int id;
	private boolean delegateFirst;
	private PluginJAR jar;

	
	
	private synchronized Class _loadClass(String clazz, boolean resolveIt)
		throws ClassNotFoundException
	{
		jar.activatePlugin();

		synchronized(this)
		{
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
				definePackage(clazz);
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

	
	private void definePackage(String clazz) throws IOException
	{
		int idx = clazz.lastIndexOf('.');
		if (idx != -1)
		{
			String name = clazz.substring(0, idx);
			if (getPackage(name) == null) definePackage(name, new JarFile(jar.getFile()).getManifest());
		}
	} 

	
	private static String getMfValue(Attributes sectionAttrs, Attributes mainAttrs, Attributes.Name name)
	{
		String value=null;
		if (sectionAttrs != null)
			value = sectionAttrs.getValue(name);
		else if (mainAttrs != null)
		{
			value = mainAttrs.getValue(name);
		}
		return value;
	}
	

	
	private void definePackage(String name, Manifest mf)
	{
		if (mf==null)
		{
			definePackage(name, null, null, null, null, null,
			null, null);
			return;
		}

		Attributes sa = mf.getAttributes(name.replace('.', '/') + '/');
		Attributes ma = mf.getMainAttributes();

		URL sealBase = null;
		if (Boolean.valueOf(getMfValue(sa, ma, Name.SEALED)).booleanValue())
		{
			try
			{
				sealBase = jar.getFile().toURL();
			}
			catch (MalformedURLException e) {}
		}

		Package pkg=definePackage(
			name,
			getMfValue(sa, ma, Name.SPECIFICATION_TITLE),
			getMfValue(sa, ma, Name.SPECIFICATION_VERSION),
			getMfValue(sa, ma, Name.SPECIFICATION_VENDOR),
			getMfValue(sa, ma, Name.IMPLEMENTATION_TITLE),
			getMfValue(sa, ma, Name.IMPLEMENTATION_VERSION),
			getMfValue(sa, ma, Name.IMPLEMENTATION_VENDOR),
			sealBase);
	} 

	
	private Class loadFromParent(String clazz)
		throws ClassNotFoundException
	{
		Class cls;

		ClassLoader parentLoader = getClass().getClassLoader();
		if (parentLoader != null)
			cls = parentLoader.loadClass(clazz);
		else
			cls = findSystemClass(clazz);

		return cls;
	} 

	
}
