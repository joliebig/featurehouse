

package org.gjt.sp.jedit.menu;


import java.awt.event.*;
import javax.swing.*;
import java.util.*;
import org.gjt.sp.jedit.browser.FileCellRenderer;
import org.gjt.sp.jedit.*;


public class RecentFilesProvider implements DynamicMenuProvider
{
	
	public boolean updateEveryTime()
	{
		return false;
	} 

	
	public void update(JMenu menu)
	{
		final View view = GUIUtilities.getView(menu);

		
		ActionListener actionListener = new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				jEdit.openFile(view,evt.getActionCommand());
				view.getStatus().setMessage(null);
			}
		}; 

		
		MouseListener mouseListener = new MouseAdapter()
		{
			public void mouseEntered(MouseEvent evt)
			{
				view.getStatus().setMessage(
					((JMenuItem)evt.getSource())
					.getActionCommand());
			}

			public void mouseExited(MouseEvent evt)
			{
				view.getStatus().setMessage(null);
			}
		}; 

		List recentVector = BufferHistory.getHistory();

		if(recentVector.size() == 0)
		{
			JMenuItem menuItem = new JMenuItem(
				jEdit.getProperty("no-recent-files.label"));
			menuItem.setEnabled(false);
			menu.add(menuItem);
			return;
		}

		Vector menuItems = new Vector();

		boolean sort = jEdit.getBooleanProperty("sortRecent");

		int maxItems = jEdit.getIntegerProperty("menu.spillover",20);

		Iterator iter = recentVector.iterator();
		while(iter.hasNext())
		{
			String path = ((BufferHistory.Entry)iter.next()).path;
			JMenuItem menuItem = new JMenuItem(MiscUtilities
				.getFileName(path));
			menuItem.setActionCommand(path);
			menuItem.addActionListener(actionListener);
			menuItem.addMouseListener(mouseListener);
			menuItem.setIcon(FileCellRenderer.fileIcon);

			if(sort)
				menuItems.addElement(menuItem);
			else
			{
				if(menu.getMenuComponentCount() >= maxItems
					&& iter.hasNext())
				{
					JMenu newMenu = new JMenu(
						jEdit.getProperty("common.more"));
					menu.add(newMenu);
					menu = newMenu;
				}

				menu.add(menuItem);
			}
		}

		if(sort)
		{
			MiscUtilities.quicksort(menuItems,
				new MiscUtilities.MenuItemCompare());
			for(int i = 0; i < menuItems.size(); i++)
			{
				if(menu.getMenuComponentCount() >= maxItems
					&& i != 0)
				{
					JMenu newMenu = new JMenu(
						jEdit.getProperty("common.more"));
					menu.add(newMenu);
					menu = newMenu;
				}

				menu.add((JMenuItem)menuItems.elementAt(i));
			}
		}
	} 
}
