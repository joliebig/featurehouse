

package org.gjt.sp.jedit;

import javax.swing.JMenuItem;
import java.util.*;
import java.io.File;

import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.gui.OptionsDialog;
import org.gjt.sp.jedit.menu.EnhancedMenu;


public abstract class EditPlugin
{
	
	
	public void start() {}
	

	
	
	public void stop() {} 

	
	
	public boolean usePluginHome()
	{
		return false;
	} 

	
	
	public static String getPluginHome(Class<? extends EditPlugin> clazz)
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if (settingsDirectory == null)
			return null;

		File file = new File(settingsDirectory, "plugins");
		return new File(file, clazz.getName()).getPath();
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
		if(jEdit.getProperty(menuProperty) != null)
		{
			String pluginName = jEdit.getProperty("plugin." +
				getClassName() + ".name");
			return new EnhancedMenu(menuProperty,pluginName,
				VFSBrowser.getActionContext());
		}

		return null;
	} 

	

	
	
	public void createMenuItems(Vector menuItems) {} 

	
	
	public void createOptionPanes(OptionsDialog optionsDialog) {} 

	

	
	PluginJAR jar;
	

	
	
	public static class Broken extends EditPlugin
	{
		public String getClassName()
		{
			return clazz;
		}

		
		Broken(PluginJAR jar, String clazz)
		{
			this.jar = jar;
			this.clazz = clazz;
		}

		
		private String clazz;
	} 

	
	
	public static class Deferred extends EditPlugin
	{
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

		
		private String clazz;
	} 
}
