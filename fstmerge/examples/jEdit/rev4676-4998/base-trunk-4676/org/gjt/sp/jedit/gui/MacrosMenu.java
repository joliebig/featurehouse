

package org.gjt.sp.jedit.gui;

import javax.swing.*;
import java.util.Collections;
import java.util.Vector;
import org.gjt.sp.jedit.msg.MacrosChanged;
import org.gjt.sp.jedit.*;

public class MacrosMenu extends EnhancedMenu implements EBComponent
{
	public MacrosMenu()
	{
		super("macros");
	}

	public void addNotify()
	{
		super.addNotify();
		EditBus.addToBus(this);
	}

	public void removeNotify()
	{
		super.removeNotify();
		EditBus.removeFromBus(this);
	}

	public void handleMessage(EBMessage msg)
	{
		if(msg instanceof MacrosChanged)
			initialized = false;
	}

	public void init()
	{
		super.init();
		updateMacrosMenu();
	}

	private void updateMacrosMenu()
	{
		
		
		
		for(int i = getMenuComponentCount() - 1; i >= 0; i--)
		{
			if(getMenuComponent(i) instanceof JSeparator)
				break;
			else
				remove(i);
		}

		int count = getMenuComponentCount();

		Vector macroVector = Macros.getMacroHierarchy();
		createMacrosMenu(this,macroVector,0);

		if(count == getMenuComponentCount())
			add(GUIUtilities.loadMenuItem("no-macros"));
	}

	private void createMacrosMenu(JMenu menu, Vector vector, int start)
	{
		Vector menuItems = new Vector();

		for(int i = start; i < vector.size(); i++)
		{
			Object obj = vector.elementAt(i);
			if(obj instanceof String)
			{
				menuItems.add(new EnhancedMenuItem(
					jEdit.getProperty(obj + ".label"),
					(String)obj,jEdit.getActionContext()));
			}
			else if(obj instanceof Vector)
			{
				Vector subvector = (Vector)obj;
				String name = (String)subvector.elementAt(0);
				JMenu submenu = new JMenu(name);
				createMacrosMenu(submenu,subvector,1);
				if(submenu.getMenuComponentCount() != 0)
					menuItems.add(submenu);
			}
		}

		Collections.sort(menuItems,new MiscUtilities.MenuItemCompare());
		for(int i = 0; i < menuItems.size(); i++)
		{
			menu.add((JMenuItem)menuItems.get(i));
		}
	}
}
