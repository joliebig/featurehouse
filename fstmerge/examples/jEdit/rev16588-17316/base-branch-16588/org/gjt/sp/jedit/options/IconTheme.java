package org.gjt.sp.jedit.options;

import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;

public class IconTheme extends Object
{
	static final String[] builtIn  = new String[] { "tango", "classic" };
	
	public static String[] builtInNames()
	{
		return builtIn;
	}

	public static String get() 
	{
		return jEdit.getProperty("icon-theme", "tango");
	}
	
	public static void set(String name)
	{
		GUIUtilities.setIconPath("jeditresource:/org/gjt/sp/jedit/icons/themes/" + name + "/");
		jEdit.setProperty("icon-theme", name);
	}
	
}
