

package org.gjt.sp.jedit;

import javax.swing.JMenuItem;
import java.util.*;
import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.gui.OptionsDialog;
import org.gjt.sp.jedit.menu.EnhancedMenu;


public abstract class EditPlugin
{
	
	
	public void start() {}
	

	
	
	public void stop() {} 

	
	
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

	
	
	public EditPlugin.JAR getJAR()
	{
		return jar;
	} 

	

	
	EditPlugin.JAR jar;
	

	
	
	public static class Broken extends EditPlugin
	{
		public String getClassName()
		{
			return clazz;
		}

		
		Broken(String clazz)
		{
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

		
		Deferred(String clazz)
		{
			this.clazz = clazz;
		}

		EditPlugin loadPluginClass()
		{
			return null;
		}

		public String toString()
		{
			return "Deferred[" + clazz + "]";
		}

		
		private String clazz;
	} 

	
	
	public static class JAR extends PluginJAR
	{
		JAR(java.io.File file)
		{
			super(file);
		}
	}
	
}
