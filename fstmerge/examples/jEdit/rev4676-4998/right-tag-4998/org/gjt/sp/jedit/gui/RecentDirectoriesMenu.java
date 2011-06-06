

package org.gjt.sp.jedit.gui;


import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.util.Vector;
import org.gjt.sp.jedit.browser.*;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.*;


public class RecentDirectoriesMenu extends EnhancedMenu
{
	
	public RecentDirectoriesMenu()
	{
		super("recent-directories");
	} 

	
	public void menuSelected(MenuEvent evt)
	{
		super.menuSelected(evt);
		final View view = GUIUtilities.getView(this);

		if(getMenuComponentCount() != 0)
			removeAll();

		
		ActionListener actionListener = new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				VFSBrowser.browseDirectory(view,evt.getActionCommand());

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

		HistoryModel model = HistoryModel.getModel("vfs.browser.path");
		if(model.getSize() == 0)
		{
			add(GUIUtilities.loadMenuItem("no-recent-dirs"));
			return;
		}

		boolean sort = jEdit.getBooleanProperty("sortRecent");

		Vector menuItems = new Vector();

		for(int i = 0; i < model.getSize(); i++)
		{
			String path = model.getItem(i);
			VFS vfs = VFSManager.getVFSForPath(path);
			JMenuItem menuItem = new JMenuItem(vfs.getFileName(path));
			menuItem.setActionCommand(path);
			menuItem.addActionListener(actionListener);
			menuItem.addMouseListener(mouseListener);
			menuItem.setIcon(FileCellRenderer.dirIcon);

			if(sort)
				menuItems.addElement(menuItem);
			else
				add(menuItem);
		}

		if(sort)
		{
			MiscUtilities.quicksort(menuItems,
				new MiscUtilities.MenuItemCompare());
			for(int i = 0; i < menuItems.size(); i++)
			{
				add((JMenuItem)menuItems.elementAt(i));
			}
		}
	} 

	public void menuDeselected(MenuEvent e) {}

	public void menuCanceled(MenuEvent e) {}
}
