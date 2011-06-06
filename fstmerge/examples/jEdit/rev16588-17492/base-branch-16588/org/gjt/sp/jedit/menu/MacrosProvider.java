

package org.gjt.sp.jedit.menu;


import javax.swing.*;
import java.util.Collections;
import java.util.Vector;
import org.gjt.sp.jedit.*;


public class MacrosProvider implements DynamicMenuProvider
{
	
	public boolean updateEveryTime()
	{
		return false;
	} 

	
	public void update(JMenu menu)
	{
		Vector macroVector = Macros.getMacroHierarchy();

		int count = menu.getMenuComponentCount();

		createMacrosMenu(menu,macroVector,0);

		if(count == menu.getMenuComponentCount())
		{
			JMenuItem mi = new JMenuItem(jEdit.getProperty(
				"no-macros.label"));
			mi.setEnabled(false);
			menu.add(mi);
		}
	} 

	
	private void createMacrosMenu(JMenu menu, Vector vector, int start)
	{
		Vector<JMenuItem> menuItems = new Vector<JMenuItem>();

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

		Collections.sort(menuItems, new MenuItemTextComparator());
		for(int i = 0; i < menuItems.size(); i++)
		{
			menu.add(menuItems.get(i));
		}
	} 
}
