

package org.gjt.sp.jedit;


import java.io.*;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.*;
import java.util.zip.*;
import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.gui.DockableWindowManager;
import org.gjt.sp.jedit.msg.PluginUpdate;
import org.gjt.sp.util.Log;



public class PluginJAR
{
	
	
	public String getPath()
	{
		return path;
	} 

	
	
	public String getCachePath()
	{
		return cachePath;
	} 

	
	
	public File getFile()
	{
		return file;
	} 

	
	
	public JARClassLoader getClassLoader()
	{
		return classLoader;
	} 

	
	
	public ZipFile getZipFile() throws IOException
	{
		if(zipFile == null)
		{
			Log.log(Log.DEBUG,this,"Opening " + path);
			zipFile = new ZipFile(path);
		}
		return zipFile;
	} 

	
	
	public ActionSet getActions()
	{
		return getActionSet();
	} 

	
	
	public ActionSet getActionSet()
	{
		return actions;
	} 

	
	
	public ActionSet getBrowserActionSet()
	{
		return browserActions;
	} 

		
	
	public boolean checkDependencies()
	{
		if(plugin == null)
			return true;

		int i = 0;

		boolean ok = true;

		String name = plugin.getClassName();

		String dep;
		while((dep = jEdit.getProperty("plugin." + name + ".depend." + i++)) != null)
		{
			int index = dep.indexOf(' ');
			if(index == -1)
			{
				Log.log(Log.ERROR,this,name + " has an invalid"
					+ " dependency: " + dep);
				ok = false;
				continue;
			}

			String what = dep.substring(0,index);
			String arg = dep.substring(index + 1);

			if(what.equals("jdk"))
			{
				if(MiscUtilities.compareStrings(
					System.getProperty("java.version"),
					arg,false) < 0)
				{
					String[] args = { arg,
						System.getProperty("java.version") };
					jEdit.pluginError(path,"plugin-error.dep-jdk",args);
					ok = false;
				}
			}
			else if(what.equals("jedit"))
			{
				if(arg.length() != 11)
				{
					Log.log(Log.ERROR,this,"Invalid jEdit version"
						+ " number: " + arg);
					ok = false;
				}

				if(MiscUtilities.compareStrings(
					jEdit.getBuild(),arg,false) < 0)
				{
					String needs = MiscUtilities.buildToVersion(arg);
					String[] args = { needs,
						jEdit.getVersion() };
					jEdit.pluginError(path,
						"plugin-error.dep-jedit",args);
					ok = false;
				}
			}
			else if(what.equals("plugin"))
			{
				int index2 = arg.indexOf(' ');
				if(index2 == -1)
				{
					Log.log(Log.ERROR,this,name 
						+ " has an invalid dependency: "
						+ dep + " (version is missing)");
					ok = false;
					continue;
				}

				String plugin = arg.substring(0,index2);
				String needVersion = arg.substring(index2 + 1);
				String currVersion = jEdit.getProperty("plugin." 
					+ plugin + ".version");

				if(currVersion == null)
				{
					String[] args = { needVersion, plugin };
					jEdit.pluginError(path,
						"plugin-error.dep-plugin.no-version",
						args);
					ok = false;
				}
				else if(MiscUtilities.compareStrings(currVersion,
					needVersion,false) < 0)
				{
					String[] args = { needVersion, plugin, currVersion };
					jEdit.pluginError(path,
						"plugin-error.dep-plugin",args);
					ok = false;
				}
				else if(jEdit.getPlugin(plugin) instanceof EditPlugin.Broken)
				{
					String[] args = { plugin };
					jEdit.pluginError(path,
						"plugin-error.dep-plugin.broken",args);
					ok = false;
				}
			}
			else if(what.equals("class"))
			{
				try
				{
					classLoader.loadClass(arg,false);
				}
				catch(Exception e)
				{
					String[] args = { arg };
					jEdit.pluginError(path,
						"plugin-error.dep-class",args);
					ok = false;
				}
			}
			else
			{
				Log.log(Log.ERROR,this,name + " has unknown"
					+ " dependency: " + dep);
				ok = false;
			}
		}

		if(!ok)
		{
			plugin = new EditPlugin.Broken(name);
			plugin.jar = (EditPlugin.JAR)this;
		}

		return ok;
	} 

	
	
	public EditPlugin getPlugin()
	{
		return plugin;
	} 

	
	
	public void activatePlugin()
	{
		synchronized(this)
		{
			if(activated)
			{
				
				return;
			}

			activated = true;

			if(!(plugin instanceof EditPlugin.Deferred && plugin != null))
				return;
		}

		String className = plugin.getClassName();

		try
		{
			Class clazz = classLoader.loadClass(className,false);
			int modifiers = clazz.getModifiers();
			if(Modifier.isInterface(modifiers)
				|| Modifier.isAbstract(modifiers)
				|| !EditPlugin.class.isAssignableFrom(clazz))
			{
				Log.log(Log.ERROR,this,"Plugin has properties but does not extend EditPlugin: "
					+ className);
				plugin = new EditPlugin.Broken(className);
				plugin.jar = (EditPlugin.JAR)this;
				return;
			}

			plugin = (EditPlugin)clazz.newInstance();
			plugin.jar = (EditPlugin.JAR)this;

			plugin.start();

			if(plugin instanceof EBPlugin)
			{
				if(jEdit.getProperty("plugin."
					+ className + ".activate")
					== null)
				{
					
					
					
					((EBComponent)plugin).handleMessage(
						new org.gjt.sp.jedit.msg.PropertiesChanged(null));
				}
				EditBus.addToBus((EBPlugin)plugin);
			}

			EditBus.send(new PluginUpdate(this,PluginUpdate.ACTIVATED));
		}
		catch(Throwable t)
		{
			plugin = new EditPlugin.Broken(className);
			plugin.jar = (EditPlugin.JAR)this;

			Log.log(Log.ERROR,this,"Error while starting plugin " + className);
			Log.log(Log.ERROR,this,t);
			String[] args = { t.toString() };
			jEdit.pluginError(path,"plugin-error.start-error",args);
		}
	} 

	
	
	public URL getDockablesURI()
	{
		return dockablesURI;
	} 

	
	
	public URL getServicesURI()
	{
		return servicesURI;
	} 

	
	public String toString()
	{
		if(plugin == null)
			return path;
		else
			return path + ",class=" + plugin.getClassName();
	} 

	

	

	
	static PluginCacheEntry getPluginCache(PluginJAR plugin)
	{
		String jarCachePath = plugin.getCachePath();
		if(jarCachePath == null)
			return null;

		DataInputStream din = null;
		try
		{
			PluginCacheEntry cache = new PluginCacheEntry();
			cache.plugin = plugin;
			cache.modTime = plugin.getFile().lastModified();
			din = new DataInputStream(
				new BufferedInputStream(
				new FileInputStream(jarCachePath)));
			if(!cache.read(din))
			{
				
				return null;
			}
			else
				return cache;
		}
		catch(FileNotFoundException fnf)
		{
			return null;
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,PluginJAR.class,io);
			return null;
		}
		finally
		{
			try
			{
				if(din != null)
					din.close();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,PluginJAR.class,io);
			}
		}
	} 

	
	static void setPluginCache(PluginJAR plugin, PluginCacheEntry cache)
	{
		String jarCachePath = plugin.getCachePath();
		if(jarCachePath == null)
			return;

		Log.log(Log.DEBUG,PluginJAR.class,"Writing " + jarCachePath);

		DataOutputStream dout = null;
		try
		{
			dout = new DataOutputStream(
				new BufferedOutputStream(
				new FileOutputStream(jarCachePath)));
			cache.write(dout);
			try
			{
				if(dout != null)
					dout.close();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,PluginJAR.class,io);
			}
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,PluginJAR.class,io);
			try
			{
				dout.close();
			}
			catch(IOException io2)
			{
				Log.log(Log.ERROR,PluginJAR.class,io2);
			}
			new File(jarCachePath).delete();
		}
	} 

	

	
	PluginJAR(File file)
	{
		this.path = file.getPath();
		String jarCacheDir = jEdit.getJARCacheDirectory();
		if(jarCacheDir != null)
		{
			cachePath = MiscUtilities.constructPath(
				jarCacheDir,file.getName() + ".summary");
		}
		this.file = file;
		classLoader = new JARClassLoader(this);
		actions = new ActionSet();
	} 

	
	void init()
	{
		PluginCacheEntry cache = getPluginCache(this);
		if(cache != null)
			loadCache(cache);
		else
		{
			try
			{
				cache = generateCache();
				setPluginCache(this,cache);
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,"Cannot load"
					+ " plugin " + plugin);
				Log.log(Log.ERROR,this,io);

				String[] args = { io.toString() };
				jEdit.pluginError(path,"plugin-error.load-error",args);
			}
		}

		classLoader.activate();

		EditBus.send(new PluginUpdate(this,PluginUpdate.LOADED));
	} 

	
	void uninit(boolean exit)
	{
		if(plugin != null)
		{
			try
			{
				plugin.stop();
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Error while "
					+ "stopping plugin:");
				Log.log(Log.ERROR,this,t);
			}
		}

		if(!exit)
		{
			classLoader.deactivate();

			if(actions != null)
				jEdit.getActionContext().removeActionSet(actions);
			if(browserActions != null)
				VFSBrowser.getActionContext().removeActionSet(browserActions);

			DockableWindowManager.unloadDockableWindows(this);
			ServiceManager.unloadServices(this);

			try
			{
				if(zipFile != null)
				{
					zipFile.close();
					zipFile = null;
				}
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,io);
			}
		}

		EditBus.send(new PluginUpdate(this,PluginUpdate.UNLOADED));
	} 

	
	void activatePluginIfNecessary()
	{
		if(!(plugin instanceof EditPlugin.Deferred && plugin != null))
			return;

		String className = plugin.getClassName();

		
		
		String activate = jEdit.getProperty("plugin."
			+ className + ".activate","startup");
		
		
		boolean load = false;

		StringTokenizer st = new StringTokenizer(activate);
		while(st.hasMoreTokens())
		{
			String prop = st.nextToken();
			boolean value = jEdit.getBooleanProperty(prop);
			if(value)
			{
				Log.log(Log.DEBUG,this,"Activating "
					+ className + " because of " + prop);
				load = true;
				break;
			}
		}

		if(load)
			activatePlugin();
	} 

	
	String[] getClasses()
	{
		return classes;
	} 

	

	

	
	private String path;
	private String cachePath;
	private File file;

	private JARClassLoader classLoader;
	private ZipFile zipFile;
	private String[] classes;
	private ActionSet actions;
	private ActionSet browserActions;

	private EditPlugin plugin;

	private URL dockablesURI;
	private URL servicesURI;

	private boolean activated;
	

	
	private void loadCache(PluginCacheEntry cache)
	{
		classes = cache.classes;

		if(cache.actionsURI != null
			&& cache.cachedActionNames != null)
		{
			actions = new ActionSet(this,
				cache.cachedActionNames,
				cache.actionsURI);
			jEdit.addActionSet(actions);
		}
		else
			actions = new ActionSet();

		if(cache.browserActionsURI != null
			&& cache.cachedBrowserActionNames != null)
		{
			browserActions = new ActionSet(this,
				cache.cachedBrowserActionNames,
				cache.browserActionsURI);
			VFSBrowser.getActionContext().addActionSet(browserActions);
		}

		if(cache.dockablesURI != null
			&& cache.cachedDockableNames != null
			&& cache.cachedDockableActionFlags != null)
		{
			dockablesURI = cache.dockablesURI;
			DockableWindowManager.cacheDockableWindows(this,
				cache.cachedDockableNames,
				cache.cachedDockableActionFlags);
		}

		if(cache.servicesURI != null
			&& cache.cachedServices != null)
		{
			servicesURI = cache.servicesURI;
			for(int i = 0; i < cache.cachedServices.length;
				i++)
			{
				ServiceManager.Descriptor d
					= cache.cachedServices[i];
				ServiceManager.registerService(d);
			}
		}

		if(cache.cachedProperties != null)
			jEdit.addProperties(cache.cachedProperties);

		if(cache.pluginClass != null)
		{
			if(actions != null)
			{
				String label = jEdit.getProperty("plugin."
					+ cache.pluginClass + ".name");
				actions.setLabel(jEdit.getProperty(
					"action-set.plugin",
					new String[] { label }));
			}
			plugin = new EditPlugin.Deferred(cache.pluginClass);
			plugin.jar = (EditPlugin.JAR)this;
		}
	} 

	
	private PluginCacheEntry generateCache() throws IOException
	{
		Properties properties = new Properties();

		LinkedList classes = new LinkedList();

		
		
		ZipFile zipFile = getZipFile();

		List plugins = new LinkedList();

		PluginCacheEntry cache = new PluginCacheEntry();
		cache.modTime = file.lastModified();
		cache.cachedProperties = new HashMap();

		Enumeration entries = zipFile.entries();
		while(entries.hasMoreElements())
		{
			ZipEntry entry = (ZipEntry)
				entries.nextElement();
			String name = entry.getName();
			String lname = name.toLowerCase();
			if(lname.equals("actions.xml"))
			{
				cache.actionsURI = classLoader.getResource(name);
			}
			if(lname.equals("browser.actions.xml"))
			{
				cache.browserActionsURI = classLoader.getResource(name);
			}
			else if(lname.equals("dockables.xml"))
			{
				dockablesURI = classLoader.getResource(name);
				cache.dockablesURI = dockablesURI;
			}
			else if(lname.equals("services.xml"))
			{
				servicesURI = classLoader.getResource(name);
				cache.servicesURI = servicesURI;
			}
			else if(lname.endsWith(".props"))
			{
				InputStream in = classLoader.getResourceAsStream(name);
				properties.load(in);
				in.close();
			}
			else if(name.endsWith(".class"))
			{
				String className = MiscUtilities
					.fileToClass(name);
				if(className.endsWith("Plugin"))
				{
					
					
					if(jEdit.getPlugin(className) != null)
					{
						jEdit.pluginError(path,
							"plugin-error.already-loaded",
							null);
					}
					else
					{
						plugins.add(className);
					}
				}
				classes.add(className);
			}
		}

		cache.cachedProperties = properties;
		jEdit.addProperties(properties);

		this.classes = cache.classes =
			(String[])classes.toArray(
			new String[classes.size()]);

		String label = null;

		Iterator iter = plugins.iterator();
		while(iter.hasNext())
		{
			String className = (String)iter.next();
			String _label = jEdit.getProperty("plugin."
				+ className + ".name");
			String version = jEdit.getProperty("plugin."
				+ className + ".version");
			if(_label == null || version == null)
			{
				Log.log(Log.NOTICE,this,"Ignoring: " + className);
			}
			else
			{
				plugin = new EditPlugin.Deferred(className);
				plugin.jar = (EditPlugin.JAR)this;
				cache.pluginClass = className;
				label = _label;
				break;
			}
		}

		if(cache.actionsURI != null)
		{
			actions = new ActionSet(this,null,cache.actionsURI);
			actions.setLabel(jEdit.getProperty(
				"action-set.plugin",
				new String[] { label }));
			actions.load();
			jEdit.addActionSet(actions);
			cache.cachedActionNames =
				actions.getCacheableActionNames();
		}

		if(cache.browserActionsURI != null)
		{
			browserActions = new ActionSet(this,null,
				cache.browserActionsURI);
			browserActions.load();
			VFSBrowser.getActionContext().addActionSet(browserActions);
			cache.cachedBrowserActionNames =
				browserActions.getCacheableActionNames();
		}

		if(dockablesURI != null)
		{
			DockableWindowManager.loadDockableWindows(this,
				dockablesURI,cache);
		}

		if(servicesURI != null)
		{
			ServiceManager.loadServices(this,servicesURI,cache);
		}

		return cache;
	} 

	

	
	
	public static class PluginCacheEntry
	{
		public static final int MAGIC = 0xB1A2E420;

		
		public PluginJAR plugin;
		public long modTime;

		public String[] classes;
		public URL actionsURI;
		public String[] cachedActionNames;
		public URL browserActionsURI;
		public String[] cachedBrowserActionNames;
		public URL dockablesURI;
		public String[] cachedDockableNames;
		public boolean[] cachedDockableActionFlags;
		public URL servicesURI;
		public ServiceManager.Descriptor[] cachedServices;

		public Map cachedProperties;
		public String pluginClass;
		

		

		
		public boolean read(DataInputStream din) throws IOException
		{
			int cacheMagic = din.readInt();
			if(cacheMagic != MAGIC)
				return false;

			String cacheBuild = readString(din);
			if(!cacheBuild.equals(jEdit.getBuild()))
				return false;

			long cacheModTime = din.readLong();
			if(cacheModTime != modTime)
				return false;

			actionsURI = readURI(din);
			cachedActionNames = readStringArray(din);

			browserActionsURI = readURI(din);
			cachedBrowserActionNames = readStringArray(din);

			dockablesURI = readURI(din);
			cachedDockableNames = readStringArray(din);
			cachedDockableActionFlags = readBooleanArray(din);

			servicesURI = readURI(din);
			int len = din.readInt();
			if(len == 0)
				cachedServices = null;
			else
			{
				cachedServices = new ServiceManager.Descriptor[len];
				for(int i = 0; i < len; i++)
				{
					ServiceManager.Descriptor d = new
						ServiceManager.Descriptor(
						readString(din),
						readString(din),
						null,
						plugin);
					cachedServices[i] = d;
				}
			}

			classes = readStringArray(din);

			cachedProperties = readMap(din);

			pluginClass = readString(din);

			return true;
		} 

		
		public void write(DataOutputStream dout) throws IOException
		{
			dout.writeInt(MAGIC);
			writeString(dout,jEdit.getBuild());

			dout.writeLong(modTime);

			writeString(dout,actionsURI);
			writeStringArray(dout,cachedActionNames);

			writeString(dout,browserActionsURI);
			writeStringArray(dout,cachedBrowserActionNames);

			writeString(dout,dockablesURI);
			writeStringArray(dout,cachedDockableNames);
			writeBooleanArray(dout,cachedDockableActionFlags);

			writeString(dout,servicesURI);
			if(cachedServices == null)
				dout.writeInt(0);
			else
			{
				dout.writeInt(cachedServices.length);
				for(int i = 0; i < cachedServices.length; i++)
				{
					writeString(dout,cachedServices[i].clazz);
					writeString(dout,cachedServices[i].name);
				}
			}

			writeStringArray(dout,classes);

			writeMap(dout,cachedProperties);

			writeString(dout,pluginClass);
		} 

		

		
		private String readString(DataInputStream din)
			throws IOException
		{
			int len = din.readInt();
			if(len == 0)
				return null;
			char[] str = new char[len];
			for(int i = 0; i < len; i++)
				str[i] = din.readChar();
			return new String(str);
		} 

		
		private URL readURI(DataInputStream din)
			throws IOException
		{
			String str = readString(din);
			if(str == null)
				return null;
			else
				return new URL(str);
		} 

		
		private String[] readStringArray(DataInputStream din)
			throws IOException
		{
			int len = din.readInt();
			if(len == 0)
				return null;
			String[] str = new String[len];
			for(int i = 0; i < len; i++)
			{
				str[i] = readString(din);
			}
			return str;
		} 

		
		private boolean[] readBooleanArray(DataInputStream din)
			throws IOException
		{
			int len = din.readInt();
			if(len == 0)
				return null;
			boolean[] bools = new boolean[len];
			for(int i = 0; i < len; i++)
			{
				bools[i] = din.readBoolean();
			}
			return bools;
		} 

		
		private Map readMap(DataInputStream din) throws IOException
		{
			HashMap returnValue = new HashMap();
			int count = din.readInt();
			for(int i = 0; i < count; i++)
			{
				String key = readString(din);
				String value = readString(din);
				if(value == null)
					value = "";
				returnValue.put(key,value);
			}
			return returnValue;
		} 

		
		private void writeString(DataOutputStream dout,
			Object obj) throws IOException
		{
			if(obj == null)
			{
				dout.writeInt(0);
			}
			else
			{
				String str = obj.toString();
				dout.writeInt(str.length());
				dout.writeChars(str);
			}
		} 

		
		private void writeStringArray(DataOutputStream dout,
			String[] str) throws IOException
		{
			if(str == null)
			{
				dout.writeInt(0);
			}
			else
			{
				dout.writeInt(str.length);
				for(int i = 0; i < str.length; i++)
				{
					writeString(dout,str[i]);
				}
			}
		} 

		
		private void writeBooleanArray(DataOutputStream dout,
			boolean[] bools) throws IOException
		{
			if(bools == null)
			{
				dout.writeInt(0);
			}
			else
			{
				dout.writeInt(bools.length);
				for(int i = 0; i < bools.length; i++)
				{
					dout.writeBoolean(bools[i]);
				}
			}
		} 

		
		private void writeMap(DataOutputStream dout, Map map)
			throws IOException
		{
			dout.writeInt(map.size());
			Iterator iter = map.keySet().iterator();
			while(iter.hasNext())
			{
				String key = (String)iter.next();
				writeString(dout,key);
				writeString(dout,map.get(key));
			}
		} 

		
	} 
}
