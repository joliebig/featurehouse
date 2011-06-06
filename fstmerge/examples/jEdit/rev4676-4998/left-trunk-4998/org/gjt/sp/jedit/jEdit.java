

package org.gjt.sp.jedit;


import bsh.UtilEvalError;
import com.microstar.xml.*;
import javax.swing.plaf.metal.*;
import javax.swing.*;
import java.awt.*;
import java.io.*;
import java.net.*;
import java.text.MessageFormat;
import java.util.*;
import org.gjt.sp.jedit.buffer.BufferIORequest;
import org.gjt.sp.jedit.buffer.KillRing;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.help.HelpViewer;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.pluginmgr.PluginManager;
import org.gjt.sp.jedit.search.SearchAndReplace;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class jEdit
{
	
	
	public static String getVersion()
	{
		return MiscUtilities.buildToVersion(getBuild());
	} 

	
	
	public static String getBuild()
	{
		
		return "04.02.10.00";
	} 

	
	
	public static void main(String[] args)
	{
		
		String javaVersion = System.getProperty("java.version");
		if(javaVersion.compareTo("1.3") < 0)
		{
			System.err.println("You are running Java version "
				+ javaVersion + ".");
			System.err.println("jEdit requires Java 1.3 or later.");
			System.exit(1);
		} 

		
		
		mainThread = Thread.currentThread();

		settingsDirectory = ".jedit";

		
		
		background = OperatingSystem.isMacOS();

		
		boolean endOpts = false;
		int level = Log.WARNING;
		String portFile = "server";
		boolean restore = true;
		boolean newView = true;
		boolean newPlainView = false;
		boolean gui = true; 
		boolean loadPlugins = true;
		boolean runStartupScripts = true;
		boolean quit = false;
		boolean wait = false;
		String userDir = System.getProperty("user.dir");

		
		String scriptFile = null;

		for(int i = 0; i < args.length; i++)
		{
			String arg = args[i];
			if(arg == null)
				continue;
			else if(arg.length() == 0)
				args[i] = null;
			else if(arg.startsWith("-") && !endOpts)
			{
				if(arg.equals("--"))
					endOpts = true;
				else if(arg.equals("-usage"))
				{
					version();
					System.err.println();
					usage();
					System.exit(1);
				}
				else if(arg.equals("-version"))
				{
					version();
					System.exit(1);
				}
				else if(arg.startsWith("-log="))
				{
					try
					{
						level = Integer.parseInt(arg.substring("-log=".length()));
					}
					catch(NumberFormatException nf)
					{
						System.err.println("Malformed option: " + arg);
					}
				}
				else if(arg.equals("-nosettings"))
					settingsDirectory = null;
				else if(arg.startsWith("-settings="))
					settingsDirectory = arg.substring(10);
				else if(arg.startsWith("-noserver"))
					portFile = null;
				else if(arg.equals("-server"))
					portFile = "server";
				else if(arg.startsWith("-server="))
					portFile = arg.substring(8);
				else if(arg.startsWith("-background"))
					background = true;
				else if(arg.startsWith("-nobackground"))
					background = false;
				else if(arg.equals("-gui"))
					gui = true;
				else if(arg.equals("-nogui"))
					gui = false;
				else if(arg.equals("-newview"))
					newView = true;
				else if(arg.equals("-newplainview"))
					newPlainView = true;
				else if(arg.equals("-reuseview"))
					newPlainView = newView = false;
				else if(arg.equals("-restore"))
					restore = true;
				else if(arg.equals("-norestore"))
					restore = false;
				else if(arg.equals("-plugins"))
					loadPlugins = true;
				else if(arg.equals("-noplugins"))
					loadPlugins = false;
				else if(arg.equals("-startupscripts"))
					runStartupScripts = true;
				else if(arg.equals("-nostartupscripts"))
					runStartupScripts = false;
				else if(arg.startsWith("-run="))
					scriptFile = arg.substring(5);
				else if(arg.equals("-wait"))
					wait = true;
				else if(arg.equals("-quit"))
					quit = true;
				else
				{
					System.err.println("Unknown option: "
						+ arg);
					usage();
					System.exit(1);
				}
				args[i] = null;
			}
		} 

		
		if(settingsDirectory != null)
		{
			settingsDirectory = MiscUtilities.constructPath(
				System.getProperty("user.home"),
				settingsDirectory);
			settingsDirectory = MiscUtilities.resolveSymlinks(
				settingsDirectory);
		}

		if(settingsDirectory != null && portFile != null)
			portFile = MiscUtilities.constructPath(settingsDirectory,portFile);
		else
			portFile = null;

		Log.init(true,level);
		

		
		if(portFile != null && new File(portFile).exists())
		{
			int port, key;
			try
			{
				BufferedReader in = new BufferedReader(new FileReader(portFile));
				String check = in.readLine();
				if(!check.equals("b"))
					throw new Exception("Wrong port file format");

				port = Integer.parseInt(in.readLine());
				key = Integer.parseInt(in.readLine());

				Socket socket = new Socket(InetAddress.getByName("127.0.0.1"),port);
				DataOutputStream out = new DataOutputStream(
					socket.getOutputStream());
				out.writeInt(key);

				String script;
				if(quit)
				{
					script = "socket.close();\n"
						+ "jEdit.exit(null,true);\n";
				}
				else
				{
					script = makeServerScript(wait,restore,
						newView,newPlainView,args,
						scriptFile);
				}

				out.writeUTF(script);

				Log.log(Log.DEBUG,jEdit.class,"Waiting for server");
				
				try
				{
					socket.getInputStream().read();
				}
				catch(Exception e)
				{
				}

				in.close();
				out.close();

				System.exit(0);
			}
			catch(Exception e)
			{
				
				
				
				Log.log(Log.NOTICE,jEdit.class,"An error occurred"
					+ " while connecting to the jEdit server instance.");
				Log.log(Log.NOTICE,jEdit.class,"This probably means that"
					+ " jEdit crashed and/or exited abnormally");
				Log.log(Log.NOTICE,jEdit.class,"the last time it was run.");
				Log.log(Log.NOTICE,jEdit.class,"If you don't"
					+ " know what this means, don't worry.");
				Log.log(Log.NOTICE,jEdit.class,e);
			}
		}

		if(quit)
		{
			
			
			System.exit(0);
		} 

		
		
		if(!new File(settingsDirectory,"nosplash").exists())
			GUIUtilities.showSplashScreen();

		
		Writer stream;
		if(settingsDirectory != null)
		{
			File _settingsDirectory = new File(settingsDirectory);
			if(!_settingsDirectory.exists())
				_settingsDirectory.mkdirs();
			File _macrosDirectory = new File(settingsDirectory,"macros");
			if(!_macrosDirectory.exists())
				_macrosDirectory.mkdir();

			String logPath = MiscUtilities.constructPath(
				settingsDirectory,"activity.log");

			backupSettingsFile(new File(logPath));

			try
			{
				stream = new BufferedWriter(new FileWriter(logPath));

				
				String lineSep = System.getProperty("line.separator");
				stream.write("Log file created on " + new Date());
				stream.write(lineSep);
				stream.write("IMPORTANT:");
				stream.write(lineSep);
				stream.write("Because updating this file after "
					+ "every log message would kill");
				stream.write(lineSep);
				stream.write("performance, it will be *incomplete* "
					+ "unless you invoke the");
				stream.write(lineSep);
				stream.write("Utilities->Troubleshooting->Update "
					+ "Activity Log on Disk command!");
				stream.write(lineSep);
			}
			catch(Exception e)
			{
				e.printStackTrace();
				stream = null;
			}
		}
		else
		{
			stream = null;
		} 

		Log.setLogWriter(stream);

		Log.log(Log.NOTICE,jEdit.class,"jEdit version " + getVersion());
		Log.log(Log.MESSAGE,jEdit.class,"Settings directory is "
			+ settingsDirectory);

		
		initMisc();
		initSystemProperties();

		GUIUtilities.advanceSplashProgress();

		GUIUtilities.init();
		BeanShell.init();

		if(jEditHome != null)
			initSiteProperties();

		initUserProperties();
		

		
		if(portFile != null)
		{
			server = new EditServer(portFile);
			if(!server.isOK())
				server = null;
		}
		else
		{
			if(background)
			{
				background = false;
				Log.log(Log.WARNING,jEdit.class,"You cannot specify both the"
					+ " -background and -noserver switches");
			}
		} 

		
		initPLAF();

		VFSManager.init();
		initResources();
		SearchAndReplace.load();

		GUIUtilities.advanceSplashProgress();

		if(loadPlugins)
			initPlugins();

		HistoryModel.loadHistory();
		BufferHistory.load();
		KillRing.load();
		propertiesChanged();

		GUIUtilities.advanceSplashProgress();

		
		sortBuffers = getBooleanProperty("sortBuffers");
		sortByName = getBooleanProperty("sortByName");

		reloadModes();

		GUIUtilities.advanceSplashProgress();
		

		
		if(OperatingSystem.hasJava14())
		{
			try
			{
				ClassLoader loader = jEdit.class.getClassLoader();
				Class clazz;
				if(loader != null)
					clazz = loader.loadClass("org.gjt.sp.jedit.Java14");
				else
					clazz = Class.forName("org.gjt.sp.jedit.Java14");
				java.lang.reflect.Method meth = clazz
					.getMethod("init",new Class[0]);
				meth.invoke(null,new Object[0]);
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,jEdit.class,e);
				System.exit(1);
			}
		} 

		
		for(int i = 0; i < jars.size(); i++)
		{
			((PluginJAR)jars.elementAt(i)).activatePluginIfNecessary();
		} 

		
		Macros.loadMacros();
		Macros.getMacroActionSet().initKeyBindings();

		if(runStartupScripts && jEditHome != null)
		{
			String path = MiscUtilities.constructPath(jEditHome,"startup");
			File file = new File(path);
			if(file.exists())
				runStartupScripts(file);
		}

		if(runStartupScripts && settingsDirectory != null)
		{
			String path = MiscUtilities.constructPath(settingsDirectory,"startup");
			File file = new File(path);
			if(!file.exists())
				file.mkdirs();
			else
				runStartupScripts(file);
		} 

		
		if(scriptFile != null)
		{
			scriptFile = MiscUtilities.constructPath(userDir,scriptFile);
			try
			{
				BeanShell.getNameSpace().setVariable("args",args);
			}
			catch(UtilEvalError e)
			{
				Log.log(Log.ERROR,jEdit.class,e);
			}
			BeanShell.runScript(null,scriptFile,null,false);
		} 

		GUIUtilities.advanceSplashProgress();

		
		finishStartup(gui,restore,userDir,args);
	} 

	

	
	
	public static final Properties getProperties()
	{
		return propMgr.getProperties();
	} 

	
	
	public static final String getProperty(String name)
	{
		return propMgr.getProperty(name);
	} 

	
	
	public static final String getProperty(String name, String def)
	{
		String value = propMgr.getProperty(name);
		if(value == null)
			return def;
		else
			return value;
	} 

	
	
	public static final String getProperty(String name, Object[] args)
	{
		if(name == null)
			return null;
		if(args == null)
			return getProperty(name);
		else
		{
			String value = getProperty(name);
			if(value == null)
				return null;
			else
				return MessageFormat.format(value,args);
		}
	} 

	
	
	public static final boolean getBooleanProperty(String name)
	{
		return getBooleanProperty(name,false);
	} 

	
	
	public static final boolean getBooleanProperty(String name, boolean def)
	{
		String value = getProperty(name);
		if(value == null)
			return def;
		else if(value.equals("true") || value.equals("yes")
			|| value.equals("on"))
			return true;
		else if(value.equals("false") || value.equals("no")
			|| value.equals("off"))
			return false;
		else
			return def;
	} 

	
	
	public static final int getIntegerProperty(String name, int def)
	{
		String value = getProperty(name);
		if(value == null)
			return def;
		else
		{
			try
			{
				return Integer.parseInt(value.trim());
			}
			catch(NumberFormatException nf)
			{
				return def;
			}
		}
	} 

	
	public static double getDoubleProperty(String name, double def)
	{
		String value = getProperty(name);
		if(value == null)
			return def;
		else
		{
			try
			{
				return Double.parseDouble(value.trim());
			}
			catch(NumberFormatException nf)
			{
				return def;
			}
		}
	}
	

	
	
	public static final Font getFontProperty(String name)
	{
		return getFontProperty(name,null);
	} 

	
	
	public static final Font getFontProperty(String name, Font def)
	{
		String family = getProperty(name);
		String sizeString = getProperty(name + "size");
		String styleString = getProperty(name + "style");

		if(family == null || sizeString == null || styleString == null)
			return def;
		else
		{
			int size, style;

			try
			{
				size = Integer.parseInt(sizeString);
			}
			catch(NumberFormatException nf)
			{
				return def;
			}

			try
			{
				style = Integer.parseInt(styleString);
			}
			catch(NumberFormatException nf)
			{
				return def;
			}

			return new Font(family,style,size);
		}
	} 

	
	
	public static Color getColorProperty(String name)
	{
		return getColorProperty(name,Color.black);
	} 

	
	
	public static Color getColorProperty(String name, Color def)
	{
		String value = getProperty(name);
		if(value == null)
			return def;
		else
			return GUIUtilities.parseColor(value,def);
	} 

	
	
	public static void setColorProperty(String name, Color value)
	{
		setProperty(name,GUIUtilities.getColorHexString(value));
	} 

	
	
	public static final void setProperty(String name, String value)
	{
		propMgr.setProperty(name,value);
	} 

	
	
	public static final void setTemporaryProperty(String name, String value)
	{
		propMgr.setTemporaryProperty(name,value);
	} 

	
	
	public static final void setBooleanProperty(String name, boolean value)
	{
		setProperty(name,value ? "true" : "false");
	} 

	
	
	public static final void setIntegerProperty(String name, int value)
	{
		setProperty(name,String.valueOf(value));
	} 

	
	public static final void setDoubleProperty(String name, double value)
	{
		setProperty(name,String.valueOf(value));
	}
	

	
	
	public static final void setFontProperty(String name, Font value)
	{
		setProperty(name,value.getFamily());
		setIntegerProperty(name + "size",value.getSize());
		setIntegerProperty(name + "style",value.getStyle());
	} 

	
	
	public static final void unsetProperty(String name)
	{
		propMgr.unsetProperty(name);
	} 

	
	
	public static final void resetProperty(String name)
	{
		propMgr.resetProperty(name);
	} 

	
	
	public static void propertiesChanged()
	{
		initKeyBindings();

		Autosave.setInterval(getIntegerProperty("autosave",30));

		saveCaret = getBooleanProperty("saveCaret");

		
		
		

		UIDefaults defaults = UIManager.getDefaults();

		
		Font font = getFontProperty("view.font");

		
		defaults.put("TextArea.font",font);
		defaults.put("TextPane.font",font);

		
		ToolTipManager.sharedInstance().setEnabled(
			jEdit.getBooleanProperty("showTooltips"));

		initProxy();

		
		Buffer buffer = buffersFirst;
		while(buffer != null)
		{
			buffer.resetCachedProperties();
			buffer.propertiesChanged();
			buffer = buffer.next;
		}

		HistoryModel.propertiesChanged();
		KillRing.propertiesChanged();

		EditBus.send(new PropertiesChanged(null));
	} 

	

	

	
	
	public static String[] getNotLoadedPluginJARs()
	{
		Vector returnValue = new Vector();

		if(jEditHome != null)
		{
			String systemPluginDir = MiscUtilities
				.constructPath(jEditHome,"jars");

			String[] list = new File(systemPluginDir).list();
			if(list != null)
				getNotLoadedPluginJARs(returnValue,systemPluginDir,list);
		}

		if(settingsDirectory != null)
		{
			String userPluginDir = MiscUtilities
				.constructPath(settingsDirectory,"jars");
			String[] list = new File(userPluginDir).list();
			if(list != null)
			{
				getNotLoadedPluginJARs(returnValue,
					userPluginDir,list);
			}
		}

		String[] _returnValue = new String[returnValue.size()];
		returnValue.copyInto(_returnValue);
		return _returnValue;
	} 

	
	
	public static EditPlugin getPlugin(String name)
	{
		return getPlugin(name, false);
	} 

	
	
	public static EditPlugin getPlugin(String name, boolean loadIfNecessary)
	{
		EditPlugin[] plugins = getPlugins();
		EditPlugin plugin = null;
		for(int i = 0; i < plugins.length; i++)
		{
			if(plugins[i].getClassName().equals(name))
				plugin = plugins[i];
			if(loadIfNecessary)
			{
				if(plugin instanceof EditPlugin.Deferred)
				{
					plugin.getPluginJAR().activatePlugin();
					plugin = plugin.getPluginJAR().getPlugin();
					break;
				}
			}
		}

		return plugin;
	} 

	
	
	public static EditPlugin[] getPlugins()
	{
		Vector vector = new Vector();
		for(int i = 0; i < jars.size(); i++)
		{
			EditPlugin plugin = ((PluginJAR)jars.elementAt(i))
				.getPlugin();
			if(plugin != null)
				vector.add(plugin);
		}

		EditPlugin[] array = new EditPlugin[vector.size()];
		vector.copyInto(array);
		return array;
	} 

	
	
	public static PluginJAR[] getPluginJARs()
	{
		PluginJAR[] array = new PluginJAR[jars.size()];
		jars.copyInto(array);
		return array;
	} 

	
	
	public static PluginJAR getPluginJAR(String path)
	{
		for(int i = 0; i < jars.size(); i++)
		{
			PluginJAR jar = (PluginJAR)jars.elementAt(i);
			if(jar.getPath().equals(path))
				return jar;
		}

		return null;
	} 

	
	
	public static void addPluginJAR(String path)
	{
		
		PluginJAR jar = new EditPlugin.JAR(new File(path));
		jars.addElement(jar);
		jar.init();

		EditBus.send(new PluginUpdate(jar,PluginUpdate.LOADED,false));
		if(!isMainThread())
		{
			EditBus.send(new DynamicMenuChanged("plugins"));
			initKeyBindings();
		}
	} 

	
	
	private static void addPluginJARsFromDirectory(String directory)
	{
		Log.log(Log.NOTICE,jEdit.class,"Loading plugins from "
			+ directory);

		File file = new File(directory);
		if(!(file.exists() && file.isDirectory()))
			return;
		String[] plugins = file.list();
		if(plugins == null)
			return;

		for(int i = 0; i < plugins.length; i++)
		{
			String plugin = plugins[i];
			if(!plugin.toLowerCase().endsWith(".jar"))
				continue;

			String path = MiscUtilities.constructPath(directory,plugin);

			
			if(plugin.equals("EditBuddy.jar")
				|| plugin.equals("PluginManager.jar")
				|| plugin.equals("Firewall.jar")
				|| plugin.equals("Tidy.jar")
				|| plugin.equals("DragAndDrop.jar"))
			{
				pluginError(path,"plugin-error.obsolete",null);
				continue;
			}

			addPluginJAR(path);
		}
	} 

	
	
	public static void removePluginJAR(PluginJAR jar, boolean exit)
	{
		if(exit)
		{
			jar.uninit(true);
		}
		else
		{
			jar.uninit(false);
			jars.removeElement(jar);
			initKeyBindings();
		}

		EditBus.send(new PluginUpdate(jar,PluginUpdate.UNLOADED,exit));
		if(!isMainThread() && !exit)
			EditBus.send(new DynamicMenuChanged("plugins"));
	} 

	

	

	
	
	public static ActionContext getActionContext()
	{
		return actionContext;
	} 

	
	
	public static void addActionSet(ActionSet actionSet)
	{
		actionContext.addActionSet(actionSet);
	} 

	
	
	public static void removeActionSet(ActionSet actionSet)
	{
		actionContext.removeActionSet(actionSet);
	} 

	
	
	public static ActionSet getBuiltInActionSet()
	{
		return builtInActionSet;
	} 

	
	
	public static ActionSet[] getActionSets()
	{
		return actionContext.getActionSets();
	} 

	
	
	public static EditAction getAction(String name)
	{
		return actionContext.getAction(name);
	} 

	
	
	public static ActionSet getActionSetForAction(String action)
	{
		return actionContext.getActionSetForAction(action);
	} 

	
	
	public static ActionSet getActionSetForAction(EditAction action)
	{
		return actionContext.getActionSetForAction(action.getName());
	} 

	
	
	public static EditAction[] getActions()
	{
		String[] names = actionContext.getActionNames();
		EditAction[] actions = new EditAction[names.length];
		for(int i = 0; i < actions.length; i++)
		{
			actions[i] = actionContext.getAction(names[i]);
			if(actions[i] == null)
				Log.log(Log.ERROR,jEdit.class,"wtf: " + names[i]);
		}
		return actions;
	} 

	
	
	public static String[] getActionNames()
	{
		return actionContext.getActionNames();
	} 

	

	

	
	
	public static void reloadModes()
	{
		
		modes = new Vector(50);

		
		if(jEditHome == null)
			loadModeCatalog("/modes/catalog",true);
		else
		{
			loadModeCatalog(MiscUtilities.constructPath(jEditHome,
				"modes","catalog"),false);
		} 

		
		if(settingsDirectory != null)
		{
			File userModeDir = new File(MiscUtilities.constructPath(
				settingsDirectory,"modes"));
			if(!userModeDir.exists())
				userModeDir.mkdirs();

			File userCatalog = new File(MiscUtilities.constructPath(
				settingsDirectory,"modes","catalog"));
			if(!userCatalog.exists())
			{
				
				try
				{
					FileWriter out = new FileWriter(userCatalog);
					out.write(jEdit.getProperty("defaultCatalog"));
					out.close();
				}
				catch(IOException io)
				{
					Log.log(Log.ERROR,jEdit.class,io);
				}
			}

			loadModeCatalog(userCatalog.getPath(),false);
		} 

		Buffer buffer = buffersFirst;
		while(buffer != null)
		{
			
			
			buffer.setMode();

			buffer = buffer.next;
		}
	} 

	
	
	public static Mode getMode(String name)
	{
		for(int i = 0; i < modes.size(); i++)
		{
			Mode mode = (Mode)modes.elementAt(i);
			if(mode.getName().equals(name))
				return mode;
		}
		return null;
	} 

	
	
	public static Mode[] getModes()
	{
		Mode[] array = new Mode[modes.size()];
		modes.copyInto(array);
		return array;
	} 

	

	

	
	
	public static Buffer openFiles(View view, String parent, String[] args)
	{
		Buffer retVal = null;
		Buffer lastBuffer = null;

		for(int i = 0; i < args.length; i++)
		{
			String arg = args[i];
			if(arg == null)
				continue;
			else if(arg.startsWith("+line:") || arg.startsWith("+marker:"))
			{
				if(lastBuffer != null)
					gotoMarker(view,lastBuffer,arg);
				continue;
			}

			lastBuffer = openFile(null,parent,arg,false,null);

			if(retVal == null && lastBuffer != null)
				retVal = lastBuffer;
		}

		if(view != null && retVal != null)
			view.setBuffer(retVal);

		return retVal;
	} 

	
	
	public static Buffer openFile(View view, String path)
	{
		return openFile(view,null,path,false,new Hashtable());
	} 

	
	
	public static Buffer openFile(View view, String parent,
		String path, boolean readOnly, boolean newFile)
	{
		return openFile(view,parent,path,newFile,new Hashtable());
	} 

	
	
	public static Buffer openFile(View view, String parent,
		String path, boolean readOnly, boolean newFile,
		Hashtable props)
	{
		return openFile(view,parent,path,newFile,props);
	} 

	
	
	public static Buffer openFile(View view, String parent,
		String path, boolean newFile, Hashtable props)
	{
		if(view != null && parent == null)
			parent = view.getBuffer().getDirectory();

		if(MiscUtilities.isURL(path))
		{
			if(MiscUtilities.getProtocolOfURL(path).equals("file"))
				path = path.substring(5);
		}

		path = MiscUtilities.constructPath(parent,path);

		synchronized(bufferListLock)
		{
			Buffer buffer = getBuffer(path);
			if(buffer != null)
			{
				if(view != null)
					view.setBuffer(buffer);

				return buffer;
			}

			if(props == null)
				props = new Hashtable();

			BufferHistory.Entry entry = BufferHistory.getEntry(path);

			if(entry != null && saveCaret && props.get(Buffer.CARET) == null)
			{
				props.put(Buffer.CARET,new Integer(entry.caret));
				
			}

			if(entry != null && props.get(Buffer.ENCODING) == null)
			{
				if(entry.encoding != null)
					props.put(Buffer.ENCODING,entry.encoding);
			}

			Buffer newBuffer = new Buffer(path,newFile,false,props);

			if(!newBuffer.load(view,false))
				return null;

			addBufferToList(newBuffer);

			EditBus.send(new BufferUpdate(newBuffer,view,BufferUpdate.CREATED));

			if(view != null)
				view.setBuffer(newBuffer);

			return newBuffer;
		}
	} 

	
	
	public static Buffer openTemporary(View view, String parent,
		String path, boolean newFile)
	{
		if(view != null && parent == null)
			parent = view.getBuffer().getDirectory();

		if(MiscUtilities.isURL(path))
		{
			if(MiscUtilities.getProtocolOfURL(path).equals("file"))
				path = path.substring(5);
		}

		path = MiscUtilities.constructPath(parent,path);

		synchronized(bufferListLock)
		{
			Buffer buffer = getBuffer(path);
			if(buffer != null)
				return buffer;

			buffer = new Buffer(path,newFile,true,new Hashtable());
			if(!buffer.load(view,false))
				return null;
			else
				return buffer;
		}
	} 

	
	
	public static void commitTemporary(Buffer buffer)
	{
		if(!buffer.isTemporary())
			return;

		addBufferToList(buffer);
		buffer.commitTemporary();

		
		EditBus.send(new BufferUpdate(buffer,null,BufferUpdate.CREATED));
		EditBus.send(new BufferUpdate(buffer,null,BufferUpdate.LOAD_STARTED));
		EditBus.send(new BufferUpdate(buffer,null,BufferUpdate.LOADED));
	} 

	
	
	public static Buffer newFile(View view)
	{
		String path;

		if(view != null && view.getBuffer() != null)
		{
			path = view.getBuffer().getDirectory();
			VFS vfs = VFSManager.getVFSForPath(path);
			
			
			if((vfs.getCapabilities() & VFS.WRITE_CAP) == 0)
				path = System.getProperty("user.home");
		}
		else
			path = null;

		return newFile(view,path);
	} 

	
	
	public static Buffer newFile(View view, String dir)
	{
		
		
		if(dir != null
			&& buffersFirst != null
			&& buffersFirst == buffersLast
			&& buffersFirst.isUntitled()
			&& !buffersFirst.isDirty())
		{
			closeBuffer(view,buffersFirst);
			
			return buffersFirst;
		}

		
		int untitledCount = 0;
		Buffer buffer = buffersFirst;
		while(buffer != null)
		{
			if(buffer.getName().startsWith("Untitled-"))
			{
				try
				{
					untitledCount = Math.max(untitledCount,
						Integer.parseInt(buffer.getName()
						.substring(9)));
				}
				catch(NumberFormatException nf)
				{
				}
			}
			buffer = buffer.next;
		}

		return openFile(view,dir,"Untitled-" + (untitledCount+1),true,null);
	} 

	

	

	
	
	public static boolean closeBuffer(View view, Buffer buffer)
	{
		
		if(buffer.isPerformingIO())
		{
			VFSManager.waitForRequests();
			if(VFSManager.errorOccurred())
				return false;
		}

		if(buffer.isDirty())
		{
			Object[] args = { buffer.getName() };
			int result = GUIUtilities.confirm(view,"notsaved",args,
				JOptionPane.YES_NO_CANCEL_OPTION,
				JOptionPane.WARNING_MESSAGE);
			if(result == JOptionPane.YES_OPTION)
			{
				if(!buffer.save(view,null,true))
					return false;

				VFSManager.waitForRequests();
				if(buffer.getBooleanProperty(BufferIORequest
					.ERROR_OCCURRED))
				{
					return false;
				}
			}
			else if(result != JOptionPane.NO_OPTION)
				return false;
		}

		_closeBuffer(view,buffer);

		return true;
	} 

	
	
	public static void _closeBuffer(View view, Buffer buffer)
	{
		if(buffer.isClosed())
		{
			
			
			return;
		}

		if(!buffer.isNewFile())
		{
			view.getEditPane().saveCaretInfo();
			Integer _caret = (Integer)buffer.getProperty(Buffer.CARET);
			int caret = (_caret == null ? 0 : _caret.intValue());

			BufferHistory.setEntry(buffer.getPath(),caret,
				(Selection[])buffer.getProperty(Buffer.SELECTION),
				buffer.getStringProperty(Buffer.ENCODING));
		}

		String path = buffer.getSymlinkPath();
		if((VFSManager.getVFSForPath(path).getCapabilities()
			& VFS.CASE_INSENSITIVE_CAP) != 0)
		{
			path = path.toLowerCase();
		}
		bufferHash.remove(path);
		removeBufferFromList(buffer);
		buffer.close();
		DisplayManager.bufferClosed(buffer);

		EditBus.send(new BufferUpdate(buffer,view,BufferUpdate.CLOSED));

		
		if(buffersFirst == null && buffersLast == null)
			newFile(view);
	} 

	
	
	public static boolean closeAllBuffers(View view)
	{
		return closeAllBuffers(view,false);
	} 

	
	
	public static boolean closeAllBuffers(View view, boolean isExiting)
	{
		boolean dirty = false;

		Buffer buffer = buffersFirst;
		while(buffer != null)
		{
			if(buffer.isDirty())
			{
				dirty = true;
				break;
			}
			buffer = buffer.next;
		}

		if(dirty)
		{
			boolean ok = new CloseDialog(view).isOK();
			if(!ok)
				return false;
		}

		
		VFSManager.waitForRequests();
		if(VFSManager.errorOccurred())
			return false;

		
		

		buffer = buffersFirst;

		
		buffersFirst = buffersLast = null;
		bufferHash.clear();
		bufferCount = 0;

		while(buffer != null)
		{
			if(!buffer.isNewFile())
			{
				Integer _caret = (Integer)buffer.getProperty(Buffer.CARET);
				int caret = (_caret == null ? 0 : _caret.intValue());
				BufferHistory.setEntry(buffer.getPath(),caret,
					(Selection[])buffer.getProperty(Buffer.SELECTION),
					buffer.getStringProperty(Buffer.ENCODING));
			}

			buffer.close();
			DisplayManager.bufferClosed(buffer);
			if(!isExiting)
			{
				EditBus.send(new BufferUpdate(buffer,view,
					BufferUpdate.CLOSED));
			}
			buffer = buffer.next;
		}

		if(!isExiting)
			newFile(view);

		return true;
	} 

	
	
	public static void saveAllBuffers(View view)
	{
		saveAllBuffers(view,jEdit.getBooleanProperty("confirmSaveAll"));
	} 

	
	
	public static void saveAllBuffers(View view, boolean confirm)
	{
		if(confirm)
		{
			int result = GUIUtilities.confirm(view,"saveall",null,
				JOptionPane.YES_NO_OPTION,
				JOptionPane.QUESTION_MESSAGE);
			if(result != JOptionPane.YES_OPTION)
				return;
		}

		Buffer current = view.getBuffer();

		Buffer buffer = buffersFirst;
		while(buffer != null)
		{
			if(buffer.isDirty())
			{
				if(buffer.isNewFile())
					view.setBuffer(buffer);
				buffer.save(view,null,true);
			}

			buffer = buffer.next;
		}

		view.setBuffer(current);
	} 

	
	
	public static void reloadAllBuffers(final View view, boolean confirm)
	{
		boolean hasDirty = false;
		Buffer[] buffers = jEdit.getBuffers();

		for(int i = 0; i < buffers.length && hasDirty == false; i++)
			hasDirty = buffers[i].isDirty();

		if(confirm && hasDirty)
		{
			int result = GUIUtilities.confirm(view,"reload-all",null,
				JOptionPane.YES_NO_OPTION,
				JOptionPane.QUESTION_MESSAGE);
			if(result != JOptionPane.YES_OPTION)
				return;
		}

		
		View _view = viewsFirst;
		while(_view != null)
		{
			EditPane[] panes = _view.getEditPanes();
			for(int i = 0; i < panes.length; i++)
			{
				panes[i].saveCaretInfo();
			}

			_view = _view.next;
		}

		for(int i = 0; i < buffers.length; i++)
		{
			Buffer buffer = buffers[i];
			buffer.load(view,true);
		}
	} 

	
	
	public static Buffer _getBuffer(String path)
	{
		
		
		if((VFSManager.getVFSForPath(path).getCapabilities()
			& VFS.CASE_INSENSITIVE_CAP) != 0)
		{
			path = path.toLowerCase();
		}

		synchronized(bufferListLock)
		{
			return (Buffer)bufferHash.get(path);
		}
	} 

	
	
	public static Buffer getBuffer(String path)
	{
		if(MiscUtilities.isURL(path))
			return _getBuffer(path);
		else
			return _getBuffer(MiscUtilities.resolveSymlinks(path));
	} 

	
	
	public static Buffer[] getBuffers()
	{
		synchronized(bufferListLock)
		{
			Buffer[] buffers = new Buffer[bufferCount];
			Buffer buffer = buffersFirst;
			for(int i = 0; i < bufferCount; i++)
			{
				buffers[i] = buffer;
				buffer = buffer.next;
			}
			return buffers;
		}
	} 

	
	
	public static int getBufferCount()
	{
		return bufferCount;
	} 

	
	
	public static Buffer getFirstBuffer()
	{
		return buffersFirst;
	} 

	
	
	public static Buffer getLastBuffer()
	{
		return buffersLast;
	} 

	
	
	public static void checkBufferStatus(View view)
	{
		
		
		
		boolean showDialogSetting = getBooleanProperty(
			"autoReloadDialog");

		
		boolean autoReloadSetting = getBooleanProperty(
			"autoReload");

		
		
		
		View _view = viewsFirst;
		while(_view != null)
		{
			EditPane[] editPanes = _view.getEditPanes();
			for(int i = 0; i < editPanes.length; i++)
			{
				editPanes[i].saveCaretInfo();
			}
			_view = _view.next;
		}

		Buffer buffer = buffersFirst;
		int[] states = new int[bufferCount];
		int i = 0;
		boolean show = false;
		while(buffer != null)
		{
			states[i] = buffer.checkFileStatus(view);

			switch(states[i])
			{
			case Buffer.FILE_CHANGED:
				if(autoReloadSetting
					&& showDialogSetting
					&& !buffer.isDirty())
				{
					buffer.load(view,true);
				}
				
			case Buffer.FILE_DELETED:
				show = true;
				break;
			}

			buffer = buffer.next;
			i++;
		}

		if(show && showDialogSetting)
			new FilesChangedDialog(view,states,autoReloadSetting);
	} 

	

	

	
	
	public static InputHandler getInputHandler()
	{
		return inputHandler;
	} 

	

	
	
	public static View newView(View view)
	{
		return newView(view,null,false);
	} 

	
	
	public static View newView(View view, Buffer buffer)
	{
		return newView(view,buffer,false);
	} 

	
	
	public static View newView(View view, Buffer buffer, boolean plainView)
	{
		View.ViewConfig config;
		if(view != null && (plainView == view.isPlainView()))
			config = view.getViewConfig();
		else
			config = new View.ViewConfig(plainView);
		return newView(view,buffer,config);
	} 

	
	
	public static View newView(View view, Buffer buffer, View.ViewConfig config)
	{
		try
		{
			if(view != null)
			{
				view.showWaitCursor();
				view.getEditPane().saveCaretInfo();
			}

			View newView = new View(buffer,config);
			addViewToList(newView);

			if(!config.plainView)
			{
				DockableWindowManager wm = newView.getDockableWindowManager();
				if(config.top != null
					&& config.top.length() != 0)
					wm.showDockableWindow(config.top);

				if(config.left != null
					&& config.left.length() != 0)
					wm.showDockableWindow(config.left);

				if(config.bottom != null
					&& config.bottom.length() != 0)
					wm.showDockableWindow(config.bottom);

				if(config.right != null
					&& config.right.length() != 0)
					wm.showDockableWindow(config.right);
			}

			newView.pack();

			if(config.width != 0 && config.height != 0)
			{
				Rectangle desired = new Rectangle(
					config.x,config.y,config.width,
					config.height);
				if(OperatingSystem.isX11() && Debug.GEOMETRY_WORKAROUND)
				{
					new GUIUtilities.UnixWorkaround(newView,
						"view",desired,config.extState);
				}
				else
				{
					newView.setBounds(desired);
					GUIUtilities.setExtendedState(newView,
						config.extState);
				}
			}
			else
				GUIUtilities.centerOnScreen(newView);

			EditBus.send(new ViewUpdate(newView,ViewUpdate.CREATED));

			newView.show();

			
			if(newView == viewsFirst)
			{
				newView.getTextArea().requestFocus();

				
				
				if(settingsDirectory != null && getBooleanProperty("firstTime"))
					new HelpViewer();
				else if(jEdit.getBooleanProperty("tip.show"))
					new TipOfTheDay(newView);

				setBooleanProperty("firstTime",false);
			}
			else
				GUIUtilities.requestFocus(newView,newView.getTextArea());

			return newView;
		}
		finally
		{
			if(view != null)
				view.hideWaitCursor();
		}
	} 

	
	
	public static void closeView(View view)
	{
		closeView(view,true);
	} 

	
	
	public static View[] getViews()
	{
		View[] views = new View[viewCount];
		View view = viewsFirst;
		for(int i = 0; i < viewCount; i++)
		{
			views[i] = view;
			view = view.next;
		}
		return views;
	} 

	
	
	public static int getViewCount()
	{
		return viewCount;
	} 

	
	
	public static View getFirstView()
	{
		return viewsFirst;
	} 

	
	
	public static View getLastView()
	{
		return viewsLast;
	} 

	
	
	public static View getActiveView()
	{
		return activeView;
	} 

	

	

	
	
	public static boolean isMainThread()
	{
		return (Thread.currentThread() == mainThread);
	} 

	
	
	public static boolean isBackgroundModeEnabled()
	{
		return background;
	} 

	
	
	public static void showMemoryDialog(View view)
	{
		Runtime rt = Runtime.getRuntime();
		int before = (int) (rt.freeMemory() / 1024);
		System.gc();
		int after = (int) (rt.freeMemory() / 1024);
		int total = (int) (rt.totalMemory() / 1024);

		JProgressBar progress = new JProgressBar(0,total);
		progress.setValue(total - after);
		progress.setStringPainted(true);
		progress.setString(jEdit.getProperty("memory-status.use",
			new Object[] { new Integer(total - after),
			new Integer(total) }));

		Object[] message = new Object[4];
		message[0] = getProperty("memory-status.gc",
			new Object[] { new Integer(after - before) });
		message[1] = Box.createVerticalStrut(12);
		message[2] = progress;
		message[3] = Box.createVerticalStrut(6);

		JOptionPane.showMessageDialog(view,message,
			jEdit.getProperty("memory-status.title"),
			JOptionPane.INFORMATION_MESSAGE);
	} 

	
	
	public static String getJEditHome()
	{
		return jEditHome;
	} 

	
	
	public static String getSettingsDirectory()
	{
		return settingsDirectory;
	} 

	
	
	public static String getJARCacheDirectory()
	{
		return jarCacheDirectory;
	} 

	
	
	public static void backupSettingsFile(File file)
	{
		if(settingsDirectory == null)
			return;

		String backupDir = MiscUtilities.constructPath(
			settingsDirectory,"settings-backup");
		File dir = new File(backupDir);
		if(!dir.exists())
			dir.mkdirs();

		
		

		MiscUtilities.saveBackup(file,5,null,"~",backupDir);
	} 

	
	
	public static void saveSettings()
	{
		if(settingsDirectory == null)
			return;

		Abbrevs.save();
		FavoritesVFS.saveFavorites();
		HistoryModel.saveHistory();
		Registers.saveRegisters();
		SearchAndReplace.save();
		BufferHistory.save();
		KillRing.save();

		File file1 = new File(MiscUtilities.constructPath(
			settingsDirectory,"#properties#save#"));
		File file2 = new File(MiscUtilities.constructPath(
			settingsDirectory,"properties"));
		if(file2.exists() && file2.lastModified() != propsModTime)
		{
			Log.log(Log.WARNING,jEdit.class,file2 + " changed"
				+ " on disk; will not save user properties");
		}
		else
		{
			backupSettingsFile(file2);

			try
			{
				OutputStream out = new FileOutputStream(file1);
				propMgr.saveUserProps(out);
				file2.delete();
				file1.renameTo(file2);
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,jEdit.class,io);
			}

			propsModTime = file2.lastModified();
		}
	} 

	
	
	public static void exit(View view, boolean reallyExit)
	{
		
		if(view == null)
			view = activeView;

		
		VFSManager.waitForRequests();

		
		EditBus.send(new EditorExitRequested(view));

		
		
		reallyExit |= !background;

		PerspectiveManager.savePerspective(false);

		
		if(!closeAllBuffers(view,reallyExit))
			return;

		
		
		if(!reallyExit)
		{
			
			
			
			view = viewsFirst;
			while(view != null)
			{
				closeView(view,false);
				view = view.next;
			}

			
			
			saveSettings();
		}
		else
		{
			
			if(view != null)
				view.close();

			
			Autosave.stop();

			
			if(server != null)
				server.stopServer();

			
			PluginJAR[] plugins = getPluginJARs();
			for(int i = 0; i < plugins.length; i++)
			{
				removePluginJAR(plugins[i],true);
			}

			
			EditBus.send(new EditorExiting(null));

			
			saveSettings();

			
			Log.closeStream();

			
			System.exit(0);
		}
	} 

	
	
	public static EditServer getEditServer()
	{
		return server;
	} 

	

	

	
	
	static void updatePosition(String oldPath, Buffer buffer)
	{
		if((VFSManager.getVFSForPath(oldPath).getCapabilities()
			& VFS.CASE_INSENSITIVE_CAP) != 0)
		{
			oldPath = oldPath.toLowerCase();
		}

		bufferHash.remove(oldPath);

		String path = buffer.getSymlinkPath();
		if((VFSManager.getVFSForPath(path).getCapabilities()
			& VFS.CASE_INSENSITIVE_CAP) != 0)
		{
			path = path.toLowerCase();
		}

		bufferHash.put(path,buffer);

		if(sortBuffers)
		{
			removeBufferFromList(buffer);
			addBufferToList(buffer);
		}
	} 

	
	
	public static void addMode(Mode mode)
	{
		
		

		modes.addElement(mode);
	} 

	
	
	 static void loadMode(Mode mode)
	{
		final String fileName = (String)mode.getProperty("file");

		Log.log(Log.NOTICE,jEdit.class,"Loading edit mode " + fileName);

		final XmlParser parser = new XmlParser();
		XModeHandler xmh = new XModeHandler(mode.getName())
		{
			public void error(String what, Object subst)
			{
				int line = parser.getLineNumber();
				int column = parser.getColumnNumber();

				String msg;

				if(subst == null)
					msg = jEdit.getProperty("xmode-error." + what);
				else
				{
					msg = jEdit.getProperty("xmode-error." + what,
						new String[] { subst.toString() });
					if(subst instanceof Throwable)
						Log.log(Log.ERROR,this,subst);
				}

				Object[] args = { fileName, new Integer(line),
					new Integer(column), msg };
				GUIUtilities.error(null,"xmode-error",args);
			}

			public TokenMarker getTokenMarker(String modeName)
			{
				Mode mode = getMode(modeName);
				if(mode == null)
					return null;
				else
					return mode.getTokenMarker();
			}
		};

		mode.setTokenMarker(xmh.getTokenMarker());

		Reader grammar = null;

		parser.setHandler(xmh);
		try
		{
			grammar = new BufferedReader(new FileReader(fileName));

			parser.parse(null, null, grammar);

			mode.setProperties(xmh.getModeProperties());
		}
		catch (Throwable e)
		{
			Log.log(Log.ERROR, jEdit.class, e);

			if (e instanceof XmlException)
			{
				XmlException xe = (XmlException) e;
				int line = xe.getLine();
				String message = xe.getMessage();

				Object[] args = { fileName, new Integer(line), null,
					message };
				GUIUtilities.error(null,"xmode-error",args);
			}
		}
		finally
		{
			try
			{
				if(grammar != null)
					grammar.close();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,jEdit.class,io);
			}
		}
	} 

	
	static void addPluginProps(Properties map)
	{
		propMgr.addPluginProps(map);
	} 

	
	static void removePluginProps(Properties map)
	{
		propMgr.removePluginProps(map);
	} 

	
	static void pluginError(String path, String messageProp,
		Object[] args)
	{
		synchronized(pluginErrorLock)
		{
			if(pluginErrors == null)
				pluginErrors = new Vector();

			ErrorListDialog.ErrorEntry newEntry =
				new ErrorListDialog.ErrorEntry(
				path,messageProp,args);

			for(int i = 0; i < pluginErrors.size(); i++)
			{
				if(pluginErrors.get(i).equals(newEntry))
					return;
			}
			pluginErrors.addElement(newEntry);

			if(startupDone)
			{
				SwingUtilities.invokeLater(new Runnable()
				{
					public void run()
					{
						showPluginErrorDialog();
					}
				});
			}
		}
	} 

	
	static void setActiveView(View view)
	{
		jEdit.activeView = view;
	} 

	

	

	
	private static String jEditHome;
	private static String settingsDirectory;
	private static String jarCacheDirectory;
	private static long propsModTime;
	private static PropertyManager propMgr;
	private static EditServer server;
	private static boolean background;
	private static ActionContext actionContext;
	private static ActionSet builtInActionSet;
	private static Vector pluginErrors;
	private static Object pluginErrorLock = new Object();
	private static Vector jars;
	private static Vector modes;
	private static boolean saveCaret;
	private static InputHandler inputHandler;
	private static JEditMetalTheme theme;

	
	private static boolean sortBuffers;
	private static boolean sortByName;
	private static int bufferCount;
	private static Buffer buffersFirst;
	private static Buffer buffersLast;
	private static Map bufferHash;

	
	private static Object bufferListLock = new Object();

	
	private static int viewCount;
	private static View viewsFirst;
	private static View viewsLast;
	private static View activeView;

	private static boolean startupDone;

	private static Thread mainThread;
	

	private jEdit() {}

	
	private static void usage()
	{
		System.out.println("Usage: jedit [<options>] [<files>]");

		System.out.println("	<file> +marker:<marker>: Positions caret"
			+ " at marker <marker>");
		System.out.println("	<file> +line:<line>: Positions caret"
			+ " at line number <line>");
		System.out.println("	--: End of options");
		System.out.println("	-background: Run in background mode");
		System.out.println("	-nobackground: Disable background mode (default)");
		System.out.println("	-gui: Only if running in background mode; open initial view (default)");
		System.out.println("	-nogui: Only if running in background mode; don't open initial view");
		System.out.println("	-log=<level>: Log messages with level equal to or higher than this to");
		System.out.println("	 standard error. <level> must be between 1 and 9. Default is 7.");
		System.out.println("	-newplainview: Client instance opens a new plain view");
		System.out.println("	-newview: Client instance opens a new view (default)");
		System.out.println("	-plugins: Load plugins (default)");
		System.out.println("	-noplugins: Don't load any plugins");
		System.out.println("	-restore: Restore previously open files (default)");
		System.out.println("	-norestore: Don't restore previously open files");
		System.out.println("	-reuseview: Client instance reuses existing view");
		System.out.println("	-quit: Quit a running instance");
		System.out.println("	-run=<script>: Run the specified BeanShell script");
		System.out.println("	-server: Read/write server info from/to $HOME/.jedit/server (default)");
		System.out.println("	-server=<name>: Read/write server info from/to $HOME/.jedit/<name>");
		System.out.println("	-noserver: Don't start edit server");
		System.out.println("	-settings=<path>: Load user-specific settings from <path>");
		System.out.println("	-nosettings: Don't load user-specific settings");
		System.out.println("	-startupscripts: Run startup scripts (default)");
		System.out.println("	-nostartupscripts: Don't run startup scripts");
		System.out.println("	-usage: Print this message and exit");
		System.out.println("	-version: Print jEdit version and exit");
		System.out.println("	-wait: Wait until the user closes the specified buffer in the server");
		System.out.println("	 instance. Does nothing if passed to the initial jEdit instance.");
		System.out.println();
		System.out.println("Report bugs to Slava Pestov <slava@jedit.org>.");
	} 

	
	private static void version()
	{
		System.out.println("jEdit " + getVersion());
	} 

	
	
	private static String makeServerScript(boolean wait,
		boolean restore, boolean newView,
		boolean newPlainView, String[] args,
		String scriptFile)
	{
		StringBuffer script = new StringBuffer();

		String userDir = System.getProperty("user.dir");

		script.append("parent = \"");
		script.append(MiscUtilities.charsToEscapes(userDir));
		script.append("\";\n");

		script.append("args = new String[");
		script.append(args.length);
		script.append("];\n");

		for(int i = 0; i < args.length; i++)
		{
			script.append("args[");
			script.append(i);
			script.append("] = ");

			if(args[i] == null)
				script.append("null");
			else
			{
				script.append('"');
				script.append(MiscUtilities.charsToEscapes(args[i]));
				script.append('"');
			}

			script.append(";\n");
		}

		script.append("view = jEdit.getLastView();\n");
		script.append("buffer = EditServer.handleClient("
			+ restore + "," + newView + "," + newPlainView +
			",parent,args);\n");
		script.append("if(buffer != null && " + wait + ") {\n");
		script.append("\tbuffer.setWaitSocket(socket);\n");
		script.append("\tdoNotCloseSocket = true;\n");
		script.append("}\n");
		script.append("if(view != jEdit.getLastView() && " + wait + ") {\n");
		script.append("\tjEdit.getLastView().setWaitSocket(socket);\n");
		script.append("\tdoNotCloseSocket = true;\n");
		script.append("}\n");
		script.append("if(doNotCloseSocket == void)\n");
		script.append("\tsocket.close();\n");

		if(scriptFile != null)
		{
			scriptFile = MiscUtilities.constructPath(userDir,scriptFile);
			script.append("BeanShell.runScript(view,\""
				+ MiscUtilities.charsToEscapes(scriptFile)
				+ "\",null,this.namespace);\n");
		}

		return script.toString();
	} 

	
	
	private static void initMisc()
	{
		jars = new Vector();
		actionContext = new ActionContext()
		{
			public void invokeAction(EventObject evt,
				EditAction action)
			{
				View view = GUIUtilities.getView(
					(Component)evt.getSource());

				boolean actionBarVisible;
				if(view.getActionBar() == null
					|| !view.getActionBar().isShowing())
					actionBarVisible = false;
				else
				{
					actionBarVisible = view.getActionBar()
						.isVisible();
				}

				view.getInputHandler().invokeAction(action);

				if(actionBarVisible)
				{
					
					ActionBar actionBar = view
						.getActionBar();
					if(actionBar != null)
						view.removeToolBar(actionBar);
				}
			}
		};

		bufferHash = new HashMap();

		inputHandler = new DefaultInputHandler(null);

		
		System.getProperties().put("java.protocol.handler.pkgs",
			"org.gjt.sp.jedit.proto|" +
			System.getProperty("java.protocol.handler.pkgs",""));

		
		String userAgent = "jEdit/" + getVersion()
			+ " (Java " + System.getProperty("java.version")
			+ ". " + System.getProperty("java.vendor")
			+ "; " + System.getProperty("os.arch") + ")";
		System.getProperties().put("http.agent",userAgent);

		
		jEditHome = System.getProperty("jedit.home");
		if(jEditHome == null)
		{
			String classpath = System
				.getProperty("java.class.path");
			int index = classpath.toLowerCase()
				.indexOf("jedit.jar");
			int start = classpath.lastIndexOf(File
				.pathSeparator,index) + 1;
			
			 if(classpath.equalsIgnoreCase("jedit.jar"))
			{
				jEditHome = System.getProperty("user.dir");
			}
			else if(index > start)
			{
				jEditHome = classpath.substring(start,
					index - 1);
			}
			else
			{
				
				
				{
					
					jEditHome = System.getProperty("user.dir");

					Log.log(Log.WARNING,jEdit.class,"jedit.jar not in class path!");
					Log.log(Log.WARNING,jEdit.class,"Assuming jEdit is installed in "
						+ jEditHome + ".");
					Log.log(Log.WARNING,jEdit.class,"Override with jedit.home "
						+ "system property.");
				}
			}
		}

		jEditHome = MiscUtilities.resolveSymlinks(jEditHome);

		Log.log(Log.MESSAGE,jEdit.class,"jEdit home directory is " + jEditHome);

		if(settingsDirectory != null)
		{
			jarCacheDirectory = MiscUtilities.constructPath(
				settingsDirectory,"jars-cache");
			new File(jarCacheDirectory).mkdirs();
		}

		
		

		
		
		EditBus.addToBus(new SettingsReloader());

		
		
		SwingUtilities.invokeLater(new Runnable()
		{
			public void run()
			{
				Thread.currentThread().setContextClassLoader(
					new JARClassLoader());
			}
		});
	} 

	
	
	private static void initSystemProperties()
	{
		propMgr = new PropertyManager();

		try
		{
			propMgr.loadSystemProps(jEdit.class.getResourceAsStream(
				"/org/gjt/sp/jedit/jedit.props"));
			propMgr.loadSystemProps(jEdit.class.getResourceAsStream(
				"/org/gjt/sp/jedit/jedit_gui.props"));
			propMgr.loadSystemProps(jEdit.class.getResourceAsStream(
				"/org/gjt/sp/jedit/jedit_keys.props"));
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,jEdit.class,
				"Error while loading system properties!");
			Log.log(Log.ERROR,jEdit.class,
				"One of the following property files could not be loaded:\n"
				+ "- jedit.props\n"
				+ "- jedit_gui.props\n"
				+ "- jedit_keys.props\n"
				+ "jedit.jar is probably corrupt.");
			Log.log(Log.ERROR,jEdit.class,e);
			System.exit(1);
		}
	} 

	
	
	private static void initSiteProperties()
	{
		
		

		String siteSettingsDirectory = MiscUtilities.constructPath(
			jEditHome, "properties");
		File siteSettings = new File(siteSettingsDirectory);

		if (!(siteSettings.exists() && siteSettings.isDirectory()))
			return;

		String[] snippets = siteSettings.list();
		if (snippets == null)
			return;

		MiscUtilities.quicksort(snippets,
			new MiscUtilities.StringICaseCompare());

		for (int i = 0; i < snippets.length; ++i)
		{
			String snippet = snippets[i];
			if(!snippet.toLowerCase().endsWith(".props"))
				continue;

			try
			{
				String path = MiscUtilities.constructPath(
					siteSettingsDirectory,snippet);
				Log.log(Log.DEBUG,jEdit.class,
					"Loading site snippet: " + path);

				propMgr.loadSystemProps(new FileInputStream(new File(path)));
			}
			catch(FileNotFoundException fnf)
			{
				Log.log(Log.DEBUG,jEdit.class,fnf);
			}
			catch(IOException e)
			{
				Log.log(Log.ERROR,jEdit.class,"Cannot load site snippet "
					+ snippet);
				Log.log(Log.ERROR,jEdit.class,e);
			}
		}
	} 

	
	private static void initResources()
	{
		builtInActionSet = new ActionSet(null,null,null,
			jEdit.class.getResource("actions.xml"));
		builtInActionSet.setLabel(getProperty("action-set.jEdit"));
		builtInActionSet.load();

		actionContext.addActionSet(builtInActionSet);

		DockableWindowManager.loadDockableWindows(null,
			jEdit.class.getResource("dockables.xml"),
			null);

		ServiceManager.loadServices(null,
			jEdit.class.getResource("services.xml"),
			null);
	} 

	
	
	private static void initPlugins()
	{
		if(jEditHome != null)
		{
			addPluginJARsFromDirectory(MiscUtilities.constructPath(
				jEditHome,"jars"));
		}

		if(settingsDirectory != null)
		{
			File jarsDirectory = new File(settingsDirectory,"jars");
			if(!jarsDirectory.exists())
				jarsDirectory.mkdir();
			addPluginJARsFromDirectory(jarsDirectory.getPath());
		}

		PluginJAR[] jars = getPluginJARs();
		for(int i = 0; i < jars.length; i++)
		{
			jars[i].checkDependencies();
		}
	} 

	
	
	private static void initUserProperties()
	{
		if(settingsDirectory != null)
		{
			File file = new File(MiscUtilities.constructPath(
				settingsDirectory,"properties"));
			propsModTime = file.lastModified();

			try
			{
				propMgr.loadUserProps(
					new FileInputStream(file));
			}
			catch(FileNotFoundException fnf)
			{
				
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,jEdit.class,e);
			}
		}
	} 

	
	private static String fontStyleToString(int style)
	{
		if(style == 0)
			return "PLAIN";
		else if(style == Font.BOLD)
			return "BOLD";
		else if(style == Font.ITALIC)
			return "ITALIC";
		else if(style == (Font.BOLD | Font.ITALIC))
			return "BOLDITALIC";
		else
			throw new RuntimeException("Invalid style: " + style);
	} 

	
	
	private static void initPLAF()
	{
		if(OperatingSystem.hasJava15())
		{
			Font primaryFont = jEdit.getFontProperty(
				"metal.primary.font");
			if(primaryFont != null)
			{
				String primaryFontString =
					primaryFont.getFamily()
					+ "-"
					+ fontStyleToString(
					primaryFont.getStyle())
					+ "-"
					+ primaryFont.getSize();

				System.getProperties().put(
					"swing.plaf.metal.controlFont",
					primaryFontString);
				System.getProperties().put(
					"swing.plaf.metal.menuFont",
					primaryFontString);
			}

			Font secondaryFont = jEdit.getFontProperty(
				"metal.secondary.font");
			if(secondaryFont != null)
			{
				String secondaryFontString =
					secondaryFont.getFamily()
					+ "-"
					+ fontStyleToString(
					secondaryFont.getStyle())
					+ "-"
					+ secondaryFont.getSize();

				System.getProperties().put(
					"swing.plaf.metal.systemFont",
					secondaryFontString);
				System.getProperties().put(
					"swing.plaf.metal.userFont",
					secondaryFontString);
			}
		}
		else
		{
			theme = new JEditMetalTheme();
			theme.propertiesChanged();
			MetalLookAndFeel.setCurrentTheme(theme);
		}

		try
		{
			String lf = getProperty("lookAndFeel");
			if(lf != null && lf.length() != 0)
				UIManager.setLookAndFeel(lf);
			else if(OperatingSystem.isMacOS())
			{
				UIManager.setLookAndFeel(UIManager
					.getSystemLookAndFeelClassName());
			}
			else
			{
				UIManager.setLookAndFeel(UIManager
					.getCrossPlatformLookAndFeelClassName());
			}
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,jEdit.class,e);
		}

		UIDefaults defaults = UIManager.getDefaults();

		
		if(jEdit.getBooleanProperty("textColors"))
		{
			Color background = new javax.swing.plaf.ColorUIResource(
				jEdit.getColorProperty("view.bgColor"));
			Color foreground = new javax.swing.plaf.ColorUIResource(
				jEdit.getColorProperty("view.fgColor"));
			Color caretColor = new javax.swing.plaf.ColorUIResource(
				jEdit.getColorProperty("view.caretColor"));
			Color selectionColor = new javax.swing.plaf.ColorUIResource(
				jEdit.getColorProperty("view.selectionColor"));

			String[] prefixes = { "TextField", "TextArea", "List", "Table" };
			for(int i = 0; i < prefixes.length; i++)
			{
				String prefix = prefixes[i];
				defaults.put(prefix + ".disabledBackground",background);
				defaults.put(prefix + ".background",background);
				defaults.put(prefix + ".disabledForeground",foreground);
				defaults.put(prefix + ".foreground",foreground);
				defaults.put(prefix + ".caretForeground",caretColor);
				defaults.put(prefix + ".selectionForeground",foreground);
				defaults.put(prefix + ".selectionBackground",selectionColor);
				
			}

			defaults.put("Tree.background",background);
			defaults.put("Tree.foreground",foreground);
			defaults.put("Tree.textBackground",background);
			defaults.put("Tree.textForeground",foreground);
			defaults.put("Tree.selectionForeground",foreground);
			defaults.put("Tree.selectionBackground",selectionColor);
		}

		defaults.remove("SplitPane.border");
		defaults.remove("SplitPaneDivider.border");
	} 

	
	
	private static void runStartupScripts(File directory)
	{
		if (!directory.isDirectory())
			return;

		File[] snippets = directory.listFiles();
		if (snippets == null)
			return;

		MiscUtilities.quicksort(snippets,
			new MiscUtilities.StringICaseCompare());

		for(int i = 0; i < snippets.length; ++i)
		{
			File snippet = snippets[i];

			Macros.Handler handler = Macros.getHandlerForPathName(
				snippet.getPath());
			if(handler == null)
				continue;

			try
			{
				Macros.Macro newMacro = handler.createMacro(
					snippet.getName(),
					snippet.getPath());
				handler.runMacro(null,newMacro,false);
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,jEdit.class,e);
			}
		}
	} 

	
	private static void initProxy()
	{
		boolean socksEnabled = jEdit.getBooleanProperty("socks.enabled");
		if(!socksEnabled)
		{
			Log.log(Log.DEBUG,jEdit.class,"SOCKS proxy disabled");
                        System.getProperties().remove("socksProxyHost");
                        System.getProperties().remove("socksProxyPort");
		}
		else
		{
			String socksHost = jEdit.getProperty("firewall.socks.host");
			if( socksHost != null )
			{
				System.setProperty("socksProxyHost", socksHost);
				Log.log(Log.DEBUG, jEdit.class,
					"SOCKS proxy enabled: " + socksHost);
                        }

			String socksPort =  jEdit.getProperty("firewall.socks.port");
			if(socksPort != null)
				System.setProperty("socksProxyPort", socksPort);
		}

		boolean httpEnabled = jEdit.getBooleanProperty("firewall.enabled");
		if (!httpEnabled)
		{
			Log.log(Log.DEBUG, jEdit.class, "HTTP proxy disabled");
			System.getProperties().remove("proxySet");
			System.getProperties().remove("proxyHost");
			System.getProperties().remove("proxyPort");
			System.getProperties().remove("http.proxyHost");
			System.getProperties().remove("http.proxyPort");
			System.getProperties().remove("http.nonProxyHosts");
			Authenticator.setDefault(null);
		}
		else
		{
			
			String host = jEdit.getProperty("firewall.host");
			if (host == null)
				return;

			System.setProperty("http.proxyHost", host);
			Log.log(Log.DEBUG, jEdit.class, "HTTP proxy enabled: " + host);
			
			String port = jEdit.getProperty("firewall.port");
			if (port != null)
				System.setProperty("http.proxyPort", port);

			
			String nonProxyHosts = jEdit.getProperty("firewall.nonProxyHosts");
			if (nonProxyHosts != null)
				System.setProperty("http.nonProxyHosts", nonProxyHosts);

			
			String username = jEdit.getProperty("firewall.user");
			String password = jEdit.getProperty("firewall.password");

			
			if(password == null)
				password = "";

			if(username == null || username.length()==0)
			{
				Log.log(Log.DEBUG, jEdit.class, "HTTP proxy without user");
				Authenticator.setDefault(new FirewallAuthenticator(null));
			}
			else
			{
				Log.log(Log.DEBUG, jEdit.class, "HTTP proxy user: " + username);
				PasswordAuthentication pw = new PasswordAuthentication(
					username,password.toCharArray()
				);
				Authenticator.setDefault(new FirewallAuthenticator(pw));
			}
		}
	} 

	
	static class FirewallAuthenticator extends Authenticator
	{
		PasswordAuthentication pw;

		public FirewallAuthenticator(PasswordAuthentication pw)
		{
			this.pw = pw;
		}

		protected PasswordAuthentication getPasswordAuthentication()
		{
			return pw;
		}
	} 

	
	private static void finishStartup(final boolean gui, final boolean restore,
		final String userDir, final String[] args)
	{
		SwingUtilities.invokeLater(new Runnable() {
			public void run()
			{
				Buffer buffer = openFiles(null,userDir,args);

				View view = null;

				boolean restoreFiles = restore
					&& jEdit.getBooleanProperty("restore")
					&& (getBufferCount() == 0
					|| jEdit.getBooleanProperty("restore.cli"));

				if(gui || getBufferCount() != 0)
				{
					view = PerspectiveManager.loadPerspective(restoreFiles);

					if(getBufferCount() == 0)
						buffer = newFile(null);

					if(view == null)
						view = newView(null,buffer);
					else if(buffer != null)
						view.setBuffer(buffer);
				}

				
				EditBus.send(new EditorStarted(null));

				VFSManager.start();

				
				if(server != null)
					server.start();

				GUIUtilities.hideSplashScreen();

				Log.log(Log.MESSAGE,jEdit.class,"Startup "
					+ "complete");

				
				if(pluginErrors != null)
				{
					showPluginErrorDialog();
				} 

				startupDone = true;

				
				
				
				
				Toolkit.getDefaultToolkit();
			}
		});
	} 

	
	private static void showPluginErrorDialog()
	{
		if(pluginErrors == null)
			return;

		String caption = getProperty(
			"plugin-error.caption" + (pluginErrors.size() == 1
			? "-1" : ""));

		Frame frame = (PluginManager.getInstance() == null
			? (Frame)viewsFirst
			: (Frame)PluginManager.getInstance());

		new ErrorListDialog(frame,
			getProperty("plugin-error.title"),
			caption,pluginErrors,true);
		pluginErrors = null;
	} 

	
	private static void getNotLoadedPluginJARs(Vector returnValue,
		String dir, String[] list)
	{
loop:		for(int i = 0; i < list.length; i++)
		{
			String name = list[i];
			if(!name.toLowerCase().endsWith(".jar"))
				continue loop;

			String path = MiscUtilities.constructPath(dir,name);

			for(int j = 0; j < jars.size(); j++)
			{
				PluginJAR jar = (PluginJAR)
					jars.elementAt(j);
				String jarPath = jar.getPath();
				String jarName = MiscUtilities.getFileName(jarPath);

				if(path.equals(jarPath))
					continue loop;
				else if(!new File(jarPath).exists()
					&& name.equals(jarName))
					continue loop;
			}

			returnValue.addElement(path);
		}
	} 

	
	private static void gotoMarker(final View view, final Buffer buffer,
		final String marker)
	{
		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				int pos;

				
				if(marker.startsWith("+line:"))
				{
					try
					{
						int line = Integer.parseInt(marker.substring(6));
						pos = buffer.getLineStartOffset(line - 1);
					}
					catch(Exception e)
					{
						return;
					}
				}
				
				else if(marker.startsWith("+marker:"))
				{
					if(marker.length() != 9)
						return;

					Marker m = buffer.getMarker(marker.charAt(8));
					if(m == null)
						return;
					pos = m.getPosition();
				}
				
				else
					throw new InternalError();

				if(view != null && view.getBuffer() == buffer)
					view.getTextArea().setCaretPosition(pos);
				else
					buffer.setIntegerProperty(Buffer.CARET,pos);
			}
		});
	} 

	
	private static void addBufferToList(Buffer buffer)
	{
		synchronized(bufferListLock)
		{
			boolean caseInsensitiveFilesystem =
				OperatingSystem.isDOSDerived()
				|| OperatingSystem.isMacOS();
			String path = buffer.getPath();

			String symlinkPath = buffer.getSymlinkPath();
			if((VFSManager.getVFSForPath(symlinkPath).getCapabilities()
				& VFS.CASE_INSENSITIVE_CAP) != 0)
			{
				symlinkPath = symlinkPath.toLowerCase();
			}

			
			
			if(viewCount <= 1 && buffersFirst != null
				&& buffersFirst == buffersLast
				&& buffersFirst.isUntitled()
				&& !buffersFirst.isDirty())
			{
				Buffer oldBuffersFirst = buffersFirst;
				buffersFirst = buffersLast = buffer;
				DisplayManager.bufferClosed(oldBuffersFirst);
				EditBus.send(new BufferUpdate(oldBuffersFirst,
					null,BufferUpdate.CLOSED));

				bufferHash.clear();

				bufferHash.put(symlinkPath,buffer);
				return;
			}

			bufferCount++;

			bufferHash.put(symlinkPath,buffer);

			if(buffersFirst == null)
			{
				buffersFirst = buffersLast = buffer;
				return;
			}
			
			else if(sortBuffers)
			{
				String str11, str12;
				if(sortByName)
				{
					str11 = buffer.getName();
					str12 = buffer.getDirectory();
				}
				else
				{
					str11 = buffer.getDirectory();
					str12 = buffer.getName();
				}

				Buffer _buffer = buffersFirst;
				while(_buffer != null)
				{
					String str21, str22;
					if(sortByName)
					{
						str21 = _buffer.getName();
						str22 = _buffer.getDirectory();
					}
					else
					{
						str21 = _buffer.getDirectory();
						str22 = _buffer.getName();
					}

					int comp = MiscUtilities.compareStrings(str11,str21,true);
					if(comp < 0 || (comp == 0 && MiscUtilities.compareStrings(str12,str22,true) < 0))
					{
						buffer.next = _buffer;
						buffer.prev = _buffer.prev;
						_buffer.prev = buffer;
						if(_buffer != buffersFirst)
							buffer.prev.next = buffer;
						else
							buffersFirst = buffer;
						return;
					}

					_buffer = _buffer.next;
				}
			} 

			buffer.prev = buffersLast;
			
			
			buffer.next = null;
			buffersLast.next = buffer;
			buffersLast = buffer;
		}
	} 

	
	private static void removeBufferFromList(Buffer buffer)
	{
		synchronized(bufferListLock)
		{
			bufferCount--;

			boolean caseInsensitiveFilesystem =
				OperatingSystem.isDOSDerived()
				|| OperatingSystem.isMacOS();
			String path = buffer.getPath();
			if(caseInsensitiveFilesystem)
				path = path.toLowerCase();

			bufferHash.remove(path);

			if(buffer == buffersFirst && buffer == buffersLast)
			{
				buffersFirst = buffersLast = null;
				return;
			}

			if(buffer == buffersFirst)
			{
				buffersFirst = buffer.next;
				buffer.next.prev = null;
			}
			else
			{
				buffer.prev.next = buffer.next;
			}

			if(buffer == buffersLast)
			{
				buffersLast = buffersLast.prev;
				buffer.prev.next = null;
			}
			else
			{
				buffer.next.prev = buffer.prev;
			}

			
			
			buffer.next = buffer.prev = null;
		}
	} 

	
	private static void addViewToList(View view)
	{
		viewCount++;

		if(viewsFirst == null)
			viewsFirst = viewsLast = view;
		else
		{
			view.prev = viewsLast;
			viewsLast.next = view;
			viewsLast = view;
		}
	} 

	
	private static void removeViewFromList(View view)
	{
		viewCount--;

		if(viewsFirst == viewsLast)
		{
			viewsFirst = viewsLast = null;
			return;
		}

		if(view == viewsFirst)
		{
			viewsFirst = view.next;
			view.next.prev = null;
		}
		else
		{
			view.prev.next = view.next;
		}

		if(view == viewsLast)
		{
			viewsLast = viewsLast.prev;
			view.prev.next = null;
		}
		else
		{
			view.next.prev = view.prev;
		}
	} 

	
	
	private static void closeView(View view, boolean callExit)
	{
		if(viewsFirst == viewsLast && callExit)
			exit(view,false); 
		else
		{
			EditBus.send(new ViewUpdate(view,ViewUpdate.CLOSED));

			view.close();
			removeViewFromList(view);

			if(view == activeView)
				activeView = null;
		}
	} 

	
	
	private static void loadModeCatalog(String path, boolean resource)
	{
		Log.log(Log.MESSAGE,jEdit.class,"Loading mode catalog file " + path);

		ModeCatalogHandler handler = new ModeCatalogHandler(
			MiscUtilities.getParentOfPath(path),resource);
		XmlParser parser = new XmlParser();
		parser.setHandler(handler);
		Reader in = null;
		try
		{
			InputStream _in;
			if(resource)
				_in = jEdit.class.getResourceAsStream(path);
			else
				_in = new FileInputStream(path);
			in = new BufferedReader(new InputStreamReader(_in));
			parser.parse(null, null, in);
		}
		catch(XmlException xe)
		{
			int line = xe.getLine();
			String message = xe.getMessage();
			Log.log(Log.ERROR,jEdit.class,path + ":" + line
				+ ": " + message);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,jEdit.class,e);
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,jEdit.class,io);
			}
		}
	} 

	
	
	private static void initKeyBindings()
	{
		inputHandler.removeAllKeyBindings();

		ActionSet[] actionSets = getActionSets();
		for(int i = 0; i < actionSets.length; i++)
		{
			actionSets[i].initKeyBindings();
		}
	} 

	
}
