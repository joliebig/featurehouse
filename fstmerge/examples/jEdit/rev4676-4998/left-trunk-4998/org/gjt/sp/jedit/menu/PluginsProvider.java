

package org.gjt.sp.jedit.menu;

import javax.swing.*;
import java.util.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;

public class PluginsProvider implements DynamicMenuProvider
{
	
	public boolean updateEveryTime()
	{
		return false;
	} 

	
	public void update(JMenu menu)
	{
		
		
		int count = 0;

		List[] letters = new List[26];
		for(int i = 0; i < letters.length; i++)
		{
			letters[i] = new ArrayList();
		}

		Vector pluginMenuItems = new Vector();

		PluginJAR[] pluginArray = jEdit.getPluginJARs();
		for(int i = 0; i < pluginArray.length; i++)
		{
			PluginJAR jar = pluginArray[i];
			EditPlugin plugin = jar.getPlugin();
			if(plugin == null)
				continue;

			JMenuItem menuItem = plugin.createMenuItems();
			if(menuItem != null)
			{
				addToLetterMap(letters,menuItem);
				count++;
			}
			
			else if(jEdit.getProperty("plugin."
				+ plugin.getClassName()
				+ ".activate") == null)
			{
				try
				{
					pluginMenuItems.clear();
					plugin.createMenuItems(pluginMenuItems);

					Iterator iter = pluginMenuItems.iterator();
					while(iter.hasNext())
					{
						addToLetterMap(letters,
							(JMenuItem)iter.next());
						count++;
					}
				}
				catch(Throwable t)
				{
					Log.log(Log.ERROR,this,
						"Error creating menu items"
						+ " for plugin");
					Log.log(Log.ERROR,this,t);
				}
			} 
		}

		if(count == 0)
		{
			JMenuItem menuItem = new JMenuItem(
				jEdit.getProperty("no-plugins.label"));
			menuItem.setEnabled(false);
			menu.add(menuItem);
			return;
		}

		
		for(int i = 0; i < letters.length; i++)
		{
			List list = letters[i];
			Collections.sort(list,new MiscUtilities
				.MenuItemCompare());
		}

		int maxItems = jEdit.getIntegerProperty("menu.spillover",20);

		
		if(count <= maxItems)
		{
			for(int i = 0; i < letters.length; i++)
			{
				Iterator iter = letters[i].iterator();
				while(iter.hasNext())
				{
					menu.add((JMenuItem)iter.next());
				}
			}

			return;
		}

		
		count = 0;
		char first = 'A';
		JMenu submenu = new JMenu();
		menu.add(submenu);

		for(int i = 0; i < letters.length; i++)
		{
			List letter = letters[i];

			if(count + letter.size() > maxItems && count != 0)
			{
				char last = (char)(i + 'A' - 1);
				if(last == first)
					submenu.setText(String.valueOf(first));
				else
					submenu.setText(first + " - " + last);
				first = (char)(char)(i + 'A');
				count = 0;
				submenu = null;
			}

			Iterator iter = letter.iterator();
			while(iter.hasNext())
			{
				if(submenu == null)
				{
					submenu = new JMenu();
					menu.add(submenu);
				}
				submenu.add((JMenuItem)iter.next());
			}

			count += letter.size();
		}

		if(submenu != null)
		{
			char last = 'Z';
			if(last == first)
				submenu.setText(String.valueOf(first));
			else
				submenu.setText(first + " - " + last);
		}
	} 

	
	private void addToLetterMap(List[] letters, JMenuItem item)
	{
		char ch = item.getText().charAt(0);
		ch = Character.toUpperCase(ch);
		if(ch < 'A' || ch > 'Z')
		{
			Log.log(Log.ERROR,this,"Plugin menu item label must "
				+ "begin with A - Z, or a - z: "
				+ item.getText());
		}
		else
			letters[ch - 'A'].add(item);
	} 
}
