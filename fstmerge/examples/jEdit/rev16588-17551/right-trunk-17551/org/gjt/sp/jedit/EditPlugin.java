

package org.gjt.sp.jedit;

import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.gui.OptionsDialog;
import org.gjt.sp.jedit.menu.EnhancedMenu;
import org.gjt.sp.util.Log;

import javax.swing.*;
import java.io.*;
import java.util.Vector;


public abstract class EditPlugin
{
	
	
	public void start() {}
	

	
	
	public void stop() {} 

	
	
	public File getPluginHome()
	{
		return getPluginHome(getClassName());
	} 

	
	
	public static File getPluginHome(Class<? extends EditPlugin> clazz)
	{
		return getPluginHome(clazz.getName());
	} 

	
	
	public static File getPluginHome(EditPlugin plugin)
	{
		return getPluginHome(plugin.getClassName());
	} 

	
	
	private static File getPluginHome(String pluginClassName)
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if (settingsDirectory == null)
			return null;

		File file = new File(settingsDirectory, "plugins");
		if (!file.isDirectory()) 
		{
			if (!file.mkdir()) 
			{
				Log.log(Log.ERROR, EditPlugin.class, "Can't create directory:" + file.getAbsolutePath());
			}
		}
		return new File(file, pluginClassName);
	} 

	
	
	public static InputStream getResourceAsStream(Class<? extends EditPlugin> clazz, String path)
	{
		return getResourceAsStream(clazz.getName(), path);
	} 

	
	
	public static InputStream getResourceAsStream(EditPlugin plugin, String path)
	{
		return getResourceAsStream(plugin.getClassName(), path);
	} 

	
	
	private static InputStream getResourceAsStream(String pluginClassName, String path)
	{
		try 
		{
			File file = getResourcePath(pluginClassName, path);
			if (file == null || !file.exists())
				return null;
			return new FileInputStream(file);
		} 
		catch (IOException e)
		{
			return null;
		}
	} 

	
	
	public static OutputStream getResourceAsOutputStream(Class<? extends EditPlugin> clazz, String path)
	{
		return getResourceAsOutputStream(clazz.getName(), path);
	} 

	
	
	public static OutputStream getResourceAsOutputStream(EditPlugin plugin, String path)
	{
		return getResourceAsOutputStream(plugin.getClassName(), path);
	} 

	
	
	private static OutputStream getResourceAsOutputStream(String pluginClassName, String path)
	{
		try 
		{
			File file = getResourcePath(pluginClassName, path);
			if (file == null)
				return null;
			File parentFile = file.getParentFile();
			if (!parentFile.exists())
			{
				if (!parentFile.mkdirs())
				{
					Log.log(Log.ERROR, EditPlugin.class, "Unable to create folder " + parentFile.getPath());
					return null;
				}
			}
			return new FileOutputStream(file);
		}
		catch (IOException e)
		{
			return null;
		}
	} 

	
	
	public static File getResourcePath(Class<? extends EditPlugin> clazz, String path)
	{
		return getResourcePath(clazz.getName(), path);
	} 

	
	
	public static File getResourcePath(EditPlugin plugin, String path)
	{
		return getResourcePath(plugin.getClassName(), path);
	} 

	
	
	private static File getResourcePath(String pluginClassName, String path)
	{
		File home = getPluginHome(pluginClassName);
		if (home == null)
			return null;
		return new File(home, path);
	} 

	
	
	public String getClassName()
	{
		return getClass().getName();
	} 

	
	
	public PluginJAR getPluginJAR()
	{
		return jar;
	} 

	
	
	public final JMenuItem createMenuItems()
	{
		if(this instanceof Broken)
			return null;

		String menuItemName = jEdit.getProperty("plugin." +
			getClassName() + ".menu-item");
		if(menuItemName != null)
			return GUIUtilities.loadMenuItem(menuItemName);

		String menuProperty = "plugin." + getClassName() + ".menu";
		String codeProperty = "plugin." + getClassName() + ".menu.code";
		if(jEdit.getProperty(menuProperty) != null
			|| jEdit.getProperty(codeProperty) != null)
		{
			String pluginName = jEdit.getProperty("plugin." +
				getClassName() + ".name");
			return new EnhancedMenu(menuProperty,pluginName);
		}

		return null;
	} 

	
	
	public final JMenuItem createBrowserMenuItems()
	{
		if(this instanceof Broken)
			return null;

		String menuItemName = jEdit.getProperty("plugin." +
			getClassName() + ".browser-menu-item");
		if(menuItemName != null)
		{
			return GUIUtilities.loadMenuItem(
				VFSBrowser.getActionContext(),
				menuItemName,
				false);
		}

		String menuProperty = "plugin." + getClassName() + ".browser-menu";
		String codeProperty = "plugin." + getClassName() + ".browser-menu.code";
		if(jEdit.getProperty(menuProperty) != null
			|| jEdit.getProperty(codeProperty) != null)
		{
			String pluginName = jEdit.getProperty("plugin." +
				getClassName() + ".name");
			return new EnhancedMenu(menuProperty,pluginName,
				VFSBrowser.getActionContext());
		}

		return null;
	} 

	

	
	
	@Deprecated
	public void createMenuItems(Vector menuItems) {} 

	
	
	@Deprecated
	public void createOptionPanes(OptionsDialog optionsDialog) {} 

	

	
	PluginJAR jar;
	

	
	
	public static class Broken extends EditPlugin
	{
		@Override
		public String getClassName()
		{
			return clazz;
		}

		
		Broken(PluginJAR jar, String clazz)
		{
			this.jar = jar;
			this.clazz = clazz;
		}

		
		private final String clazz;
	} 

	
	
	public static class Deferred extends EditPlugin
	{
		@Override
		public String getClassName()
		{
			return clazz;
		}

		
		Deferred(PluginJAR jar, String clazz)
		{
			this.jar = jar;
			this.clazz = clazz;
		}

		EditPlugin loadPluginClass()
		{
			return null;
		}

		public String toString()
		{
			return "Deferred[" + clazz + ']';
		}

		
		private final String clazz;
	} 
}
