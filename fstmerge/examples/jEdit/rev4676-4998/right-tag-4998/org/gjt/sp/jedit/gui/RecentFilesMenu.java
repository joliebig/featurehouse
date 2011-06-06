

package org.gjt.sp.jedit.gui;


import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.util.Vector;
import org.gjt.sp.jedit.browser.FileCellRenderer;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.*;


public class RecentFilesMenu extends EnhancedMenu
{
	
	public RecentFilesMenu()
	{
		super("recent-files");
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

		Vector recentVector = BufferHistory.getBufferHistory();

		if(recentVector.size() == 0)
		{
			add(GUIUtilities.loadMenuItem("no-recent-files"));
			return;
		}

		Vector menuItems = new Vector();
		boolean sort = jEdit.getBooleanProperty("sortRecent");

		
		int recentFileCount = Math.min(recentVector.size(),
			jEdit.getIntegerProperty("history",25));

		for(int i = recentVector.size() - 1;
			i >= recentVector.size() - recentFileCount;
			i--)
		{
			String path = ((BufferHistory.Entry)recentVector
				.elementAt(i)).path;
			VFS vfs = VFSManager.getVFSForPath(path);
			JMenuItem menuItem = new JMenuItem(vfs.getFileName(path));
			menuItem.setActionCommand(path);
			menuItem.addActionListener(actionListener);
			menuItem.addMouseListener(mouseListener);
			menuItem.setIcon(FileCellRenderer.fileIcon);

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
