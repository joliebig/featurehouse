

package org.gjt.sp.jedit.menu;


import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import org.gjt.sp.jedit.browser.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;


public class FavoritesProvider implements DynamicMenuProvider
{
	
	public boolean updateEveryTime()
	{
		return false;
	} 

	
	public void update(JMenu menu)
	{
		final View view = GUIUtilities.getView(menu);

		
		ActionListener fileListener = new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				jEdit.openFile(view,evt.getActionCommand());
			}
		};

		ActionListener dirListener = new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				VFSBrowser.browseDirectory(view,
					evt.getActionCommand());
			}
		}; 

		VFS.DirectoryEntry[] favorites
			= FavoritesVFS.getFavorites();
		if(favorites.length == 0)
		{
			JMenuItem mi = new JMenuItem(
				jEdit.getProperty(
				"vfs.browser.favorites"
				+ ".no-favorites.label"));
			mi.setEnabled(false);
			menu.add(mi);
		}
		else
		{
			MiscUtilities.quicksort(favorites,
				new VFS.DirectoryEntryCompare(
				jEdit.getBooleanProperty("vfs.browser.sortMixFilesAndDirs"),
				jEdit.getBooleanProperty("vfs.browser.sortIgnoreCase")));
			for(int i = 0; i < favorites.length; i++)
			{
				VFS.DirectoryEntry favorite
					= favorites[i];
				JMenuItem mi = new JMenuItem(favorite.path);
				mi.setIcon(FileCellRenderer
					.getIconForFile(
					favorite,false));
				if(favorite.type ==
					VFS.DirectoryEntry.FILE)
				{
					mi.addActionListener(fileListener);
				}
				else
				{
					mi.addActionListener(dirListener);
				}
				menu.add(mi);
			}
		}
	} 

	
	private String dir;
	
}
