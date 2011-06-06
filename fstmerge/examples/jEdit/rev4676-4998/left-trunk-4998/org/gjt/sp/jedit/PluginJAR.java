

package org.gjt.sp.jedit;


import javax.swing.SwingUtilities;
import java.io.*;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.util.*;
import java.util.zip.*;
import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.gui.DockableWindowManager;
import org.gjt.sp.jedit.msg.*;
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

	
	
	public synchronized ZipFile getZipFile() throws IOException
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
		boolean optional = false;

		String name = plugin.getClassName();

		String dep;
		while((dep = jEdit.getProperty("plugin." + name + ".depend." + i++)) != null)
		{
			if(dep.startsWith("optional "))
			{
				optional = true;
				dep = dep.substring("optional ".length());
			}

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
				if(!optional && MiscUtilities.compareStrings(
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

				if(!optional && MiscUtilities.compareStrings(
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

				String pluginName = arg.substring(0,index2);
				String needVersion = arg.substring(index2 + 1);
				String currVersion = jEdit.getProperty("plugin." 
					+ pluginName + ".version");

				EditPlugin plugin = jEdit.getPlugin(pluginName);
				if(plugin == null)
				{
					if(!optional)
					{
						String[] args = { needVersion,
							pluginName };
						jEdit.pluginError(path,
							"plugin-error.dep-plugin.no-version",
							args);
						ok = false;
					}
				}
				else if(MiscUtilities.compareStrings(
					currVersion,needVersion,false) < 0)
				{
					if(!optional)
					{
						String[] args = { needVersion,
							pluginName, currVersion };
						jEdit.pluginError(path,
							"plugin-error.dep-plugin",args);
						ok = false;
					}
				}
				else if(plugin instanceof EditPlugin.Broken)
				{
					if(!optional)
					{
						String[] args = { pluginName };
						jEdit.pluginError(path,
							"plugin-error.dep-plugin.broken",args);
						ok = false;
					}
				}
				else
				{
					PluginJAR jar = plugin.getPluginJAR();
					jar.theseRequireMe.add(path);
					weRequireThese.add(jar.getPath());
				}
			}
			else if(what.equals("class"))
			{
				if(!optional)
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
			}
			else
			{
				Log.log(Log.ERROR,this,name + " has unknown"
					+ " dependency: " + dep);
				ok = false;
			}
		}

		
		
		String jars = jEdit.getProperty("plugin."
			+ plugin.getClassName() + ".jars");
		if(jars != null)
		{
			String dir = MiscUtilities.getParentOfPath(path);

			StringTokenizer st = new StringTokenizer(jars);
			while(st.hasMoreTokens())
			{
				String jarPath = MiscUtilities.constructPath(
					dir,st.nextToken());
				PluginJAR jar = jEdit.getPluginJAR(jarPath);
				if(jar == null)
				{
					String[] args = { jarPath };
					jEdit.pluginError(path,
						"plugin-error.missing-jar",args);
					ok = false;
				}
				else
				{
					weRequireThese.add(jarPath);
					jar.theseRequireMe.add(path);
				}
			}
		}

		if(!ok)
			breakPlugin();

		return ok;
	} 

	
	
	public String[] getDependentPlugins()
	{
		return (String[])theseRequireMe.toArray(
			new String[theseRequireMe.size()]);
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

			if(!(plugin instanceof EditPlugin.Deferred))
				return;

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
					breakPlugin();
					return;
				}

				plugin = (EditPlugin)clazz.newInstance();
				plugin.jar = (EditPlugin.JAR)this;
			}
			catch(Throwable t)
			{
				breakPlugin();

				Log.log(Log.ERROR,this,"Error while starting plugin " + className);
				Log.log(Log.ERROR,this,t);
				String[] args = { t.toString() };
				jEdit.pluginError(path,"plugin-error.start-error",args);

				return;
			}
		}

		if(jEdit.isMainThread()
			|| SwingUtilities.isEventDispatchThread())
		{
			startPlugin();
		}
		else
		{
			
			startPluginLater();
		}

		EditBus.send(new PluginUpdate(this,PluginUpdate.ACTIVATED,false));
	} 

	
	
	public void activatePluginIfNecessary()
	{
		if(!(plugin instanceof EditPlugin.Deferred && plugin != null))
			return;

		String className = plugin.getClassName();

		
		
		String activate = jEdit.getProperty("plugin."
			+ className + ".activate");

		if(activate == null)
		{
			
			if(!jEdit.isMainThread())
			{
				breakPlugin();

				jEdit.pluginError(path,"plugin-error.not-42",null);
			}
			else
				activatePlugin();
		}
		else
		{
			

			
			
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
	} 

	
	
	public void deactivatePlugin(boolean exit)
	{
		synchronized(this)
		{
			if(!activated)
				return;

			activated = false;

			if(!exit)
			{
				
				
				
				Buffer buffer = jEdit.getFirstBuffer();
				while(buffer != null)
				{
					if(buffer.getFoldHandler() != null
						&& buffer.getFoldHandler().getClass()
						.getClassLoader() == classLoader)
					{
						buffer.setFoldHandler(
							new DummyFoldHandler());
					}
					buffer = buffer.getNext();
				}
			}

			if(plugin != null && !(plugin instanceof EditPlugin.Broken))
			{
				if(plugin instanceof EBPlugin)
					EditBus.removeFromBus((EBPlugin)plugin);

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

				plugin = new EditPlugin.Deferred(
					plugin.getClassName());
				plugin.jar = (EditPlugin.JAR)this;

				EditBus.send(new PluginUpdate(this,
					PluginUpdate.DEACTIVATED,exit));

				if(!exit)
				{
					
					String activate = jEdit.getProperty("plugin."
						+ plugin.getClassName() + ".activate");

					if(activate == null)
					{
						breakPlugin();
						jEdit.pluginError(path,"plugin-error.not-42",null);
					}
				}
			}
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
			if(cache.read(din))
				return cache;
			else
			{
				
				return null;
			}
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
			dout.close();
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
		boolean initialized = false;

		PluginCacheEntry cache = getPluginCache(this);
		if(cache != null)
		{
			loadCache(cache);
			classLoader.activate();
			initialized = true;
		}
		else
		{
			try
			{
				cache = generateCache();
				if(cache != null)
				{
					setPluginCache(this,cache);
					classLoader.activate();
					initialized = true;
				}
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,"Cannot load"
					+ " plugin " + path);
				Log.log(Log.ERROR,this,io);

				String[] args = { io.toString() };
				jEdit.pluginError(path,"plugin-error.load-error",args);

				uninit(false);
			}
		}
	} 

	
	void uninit(boolean exit)
	{
		deactivatePlugin(exit);

		if(!exit)
		{
			Iterator iter = weRequireThese.iterator();
			while(iter.hasNext())
			{
				String path = (String)iter.next();
				PluginJAR jar = jEdit.getPluginJAR(path);
				if(jar != null)
					jar.theseRequireMe.remove(this.path);
			}

			classLoader.deactivate();
			BeanShell.resetClassManager();

			if(actions != null)
				jEdit.getActionContext().removeActionSet(actions);
			if(browserActions != null)
				VFSBrowser.getActionContext().removeActionSet(browserActions);

			DockableWindowManager.unloadDockableWindows(this);
			ServiceManager.unloadServices(this);

			jEdit.removePluginProps(properties);

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
	private Properties properties;
	private String[] classes;
	private ActionSet actions;
	private ActionSet browserActions;

	private EditPlugin plugin;

	private URL dockablesURI;
	private URL servicesURI;

	private boolean activated;

	private List theseRequireMe = new LinkedList();
	private List weRequireThese = new LinkedList();
	

	
	private void actionsPresentButNotCoreClass()
	{
		Log.log(Log.WARNING,this,getPath() + " has an actions.xml but no plugin core class");
		actions.setLabel("MISSING PLUGIN CORE CLASS");
	} 

	
	private void loadCache(PluginCacheEntry cache)
	{
		classes = cache.classes;

		
		if(cache.cachedProperties != null)
		{
			properties = cache.cachedProperties;
			jEdit.addPluginProps(cache.cachedProperties);
		}

		if(cache.actionsURI != null
			&& cache.cachedActionNames != null)
		{
			actions = new ActionSet(this,
				cache.cachedActionNames,
				cache.cachedActionToggleFlags,
				cache.actionsURI);
		}

		if(cache.browserActionsURI != null
			&& cache.cachedBrowserActionNames != null)
		{
			browserActions = new ActionSet(this,
				cache.cachedBrowserActionNames,
				cache.cachedBrowserActionToggleFlags,
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

		if(actions.size() != 0)
			jEdit.addActionSet(actions);

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

		if(cache.pluginClass != null)
		{
			
			
			if(jEdit.getPlugin(cache.pluginClass) != null)
			{
				jEdit.pluginError(path,
					"plugin-error.already-loaded",
					null);
				uninit(false);
			}
			else
			{
				String label = jEdit.getProperty(
					"plugin." + cache.pluginClass
					+ ".name");
				actions.setLabel(jEdit.getProperty(
					"action-set.plugin",
					new String[] { label }));
				plugin = new EditPlugin.Deferred(
					cache.pluginClass);
				plugin.jar = (EditPlugin.JAR)this;
			}
		}
		else
		{
			if(actions.size() != 0)
				actionsPresentButNotCoreClass();
		}
	} 

	
	private PluginCacheEntry generateCache() throws IOException
	{
		properties = new Properties();

		LinkedList classes = new LinkedList();

		ZipFile zipFile = getZipFile();

		List plugins = new LinkedList();

		PluginCacheEntry cache = new PluginCacheEntry();
		cache.modTime = file.lastModified();
		cache.cachedProperties = new Properties();

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
			else if(lname.equals("browser.actions.xml"))
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
					plugins.add(className);
				}
				classes.add(className);
			}
		}

		cache.cachedProperties = properties;
		jEdit.addPluginProps(properties);

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
				Log.log(Log.WARNING,this,"Ignoring: "
					+ className);
			}
			else
			{
				cache.pluginClass = className;

				
				
				if(jEdit.getPlugin(className) != null)
				{
					jEdit.pluginError(path,
						"plugin-error.already-loaded",
						null);
					return null;
				}
				else
				{
					plugin = new EditPlugin.Deferred(
						className);
					plugin.jar = (EditPlugin.JAR)this;
					label = _label;
				}

				break;
			}
		}

		if(cache.actionsURI != null)
		{
			actions = new ActionSet(this,null,null,
				cache.actionsURI);
			actions.load();
			cache.cachedActionNames =
				actions.getCacheableActionNames();
			cache.cachedActionToggleFlags = new boolean[
				cache.cachedActionNames.length];
			for(int i = 0; i < cache.cachedActionNames.length; i++)
			{
				 cache.cachedActionToggleFlags[i]
				 	= jEdit.getBooleanProperty(
					cache.cachedActionNames[i]
					+ ".toggle");
			}
		}

		if(cache.browserActionsURI != null)
		{
			browserActions = new ActionSet(this,null,null,
				cache.browserActionsURI);
			browserActions.load();
			VFSBrowser.getActionContext().addActionSet(browserActions);
			cache.cachedBrowserActionNames =
				browserActions.getCacheableActionNames();
			cache.cachedBrowserActionToggleFlags = new boolean[
				cache.cachedBrowserActionNames.length];
			for(int i = 0;
				i < cache.cachedBrowserActionNames.length;
				i++)
			{
				 cache.cachedBrowserActionToggleFlags[i]
				 	= jEdit.getBooleanProperty(
					cache.cachedBrowserActionNames[i]
					+ ".toggle");
			}
		}

		if(dockablesURI != null)
		{
			DockableWindowManager.loadDockableWindows(this,
				dockablesURI,cache);
		}

		if(actions.size() != 0)
		{
			if(label != null)
			{
				actions.setLabel(jEdit.getProperty(
					"action-set.plugin",
					new String[] { label }));
			}
			else
				actionsPresentButNotCoreClass();

			jEdit.addActionSet(actions);
		}

		if(servicesURI != null)
		{
			ServiceManager.loadServices(this,servicesURI,cache);
		}

		return cache;
	} 

	
	private void startPlugin()
	{
		try
		{
			plugin.start();
		}
		catch(Throwable t)
		{
			breakPlugin();

			Log.log(Log.ERROR,PluginJAR.this,
				"Error while starting plugin " + plugin.getClassName());
			Log.log(Log.ERROR,PluginJAR.this,t);
			String[] args = { t.toString() };
			jEdit.pluginError(path,"plugin-error.start-error",args);
		}

		if(plugin instanceof EBPlugin)
		{
			if(jEdit.getProperty("plugin."
				+ plugin.getClassName() + ".activate")
				== null)
			{
				
				
				
				((EBComponent)plugin).handleMessage(
					new org.gjt.sp.jedit.msg.PropertiesChanged(null));
			}
			EditBus.addToBus((EBPlugin)plugin);
		}

		
		
		
		Buffer buffer = jEdit.getFirstBuffer();
		while(buffer != null)
		{
			FoldHandler handler =
				FoldHandler.getFoldHandler(
				buffer.getStringProperty("folding"));
			
			if(buffer.getFoldHandler() != null
				&& handler != null
				&& handler != buffer.getFoldHandler())
			{
				buffer.setFoldHandler(handler);
			}
			buffer = buffer.getNext();
		}
	} 

	
	private void startPluginLater()
	{
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				if(!activated)
					return;

				startPlugin();
			}
		});
	} 

	
	private void breakPlugin()
	{
		plugin = new EditPlugin.Broken(plugin.getClassName());
		plugin.jar = (EditPlugin.JAR)this;

		
		
		uninit(false);
		
		jEdit.addPluginProps(properties);
	} 

	

	
	
	public static class PluginCacheEntry
	{
		public static final int MAGIC = 0xB7A2E420;

		
		public PluginJAR plugin;
		public long modTime;

		public String[] classes;
		public URL actionsURI;
		public String[] cachedActionNames;
		public boolean[] cachedActionToggleFlags;
		public URL browserActionsURI;
		public String[] cachedBrowserActionNames;
		public boolean[] cachedBrowserActionToggleFlags;
		public URL dockablesURI;
		public String[] cachedDockableNames;
		public boolean[] cachedDockableActionFlags;
		public URL servicesURI;
		public ServiceManager.Descriptor[] cachedServices;

		public Properties cachedProperties;
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
			cachedActionToggleFlags = readBooleanArray(din);

			browserActionsURI = readURI(din);
			cachedBrowserActionNames = readStringArray(din);
			cachedBrowserActionToggleFlags = readBooleanArray(din);

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
			writeBooleanArray(dout,cachedActionToggleFlags);

			writeString(dout,browserActionsURI);
			writeStringArray(dout,cachedBrowserActionNames);
			writeBooleanArray(dout,cachedBrowserActionToggleFlags);

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

		
		private Properties readMap(DataInputStream din)
			throws IOException
		{
			Properties returnValue = new Properties();
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
