

package org.gjt.sp.jedit.gui;


import javax.swing.event.*;
import javax.swing.*;
import java.util.StringTokenizer;
import org.gjt.sp.jedit.*;


public class EnhancedMenu extends JMenu implements MenuListener
{
	
	public EnhancedMenu(String name)
	{
		this(name,jEdit.getProperty(name.concat(".label")),
			jEdit.getActionContext());
	} 

	
	public EnhancedMenu(String name, String label)
	{
		this(name,label,jEdit.getActionContext());
	} 

	
	public EnhancedMenu(String name, String label, ActionContext context)
	{
		this._name = name;
		this.context = context;
		if(label == null)
			label = name;

		char mnemonic;
		int index = label.indexOf('$');
		if(index != -1 && label.length() - index > 1)
		{
			mnemonic = Character.toLowerCase(label.charAt(index + 1));
			label = label.substring(0,index).concat(label.substring(++index));
		}
		else
			mnemonic = '\0';

		setText(label);
		if(!OperatingSystem.isMacOS())
			setMnemonic(mnemonic);

		addMenuListener(this);
		
	} 

	
	public void menuSelected(MenuEvent evt)
	{
		init();
	} 

	public void menuDeselected(MenuEvent e) {}

	public void menuCanceled(MenuEvent e) {}

	
	public void init()
	{
		if(initialized)
			return;

		initialized = true;

		String menuItems = jEdit.getProperty(_name);
		if(menuItems != null)
		{
			StringTokenizer st = new StringTokenizer(menuItems);
			while(st.hasMoreTokens())
			{
				String menuItemName = st.nextToken();
				if(menuItemName.equals("-"))
					addSeparator();
				else
					add(GUIUtilities.loadMenuItem(context,menuItemName,true));
			}
		}
	} 

	protected String _name;
	protected ActionContext context;
	protected boolean initialized;
}
