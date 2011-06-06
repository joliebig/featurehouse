

package org.gjt.sp.jedit.menu;


import javax.swing.*;
import java.awt.event.*;
import java.io.File;
import java.util.Arrays;

import org.gjt.sp.jedit.browser.*;
import org.gjt.sp.jedit.io.FileVFS;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.StandardUtilities;



public class DirectoryProvider implements DynamicMenuProvider
{
	
	public DirectoryProvider(String dir)
	{
		this.dir = dir;
	} 

	
	public boolean updateEveryTime()
	{
		return true;
	} 

	
	public void update(JMenu menu)
	{
		final View view = GUIUtilities.getView(menu);

		String path;
		if(dir == null)
		{
			path = view.getBuffer().getDirectory();
		}
		else
			path = dir;

		JMenuItem mi = new JMenuItem(path + ':');
		mi.setActionCommand(path);
		mi.setIcon(FileCellRenderer.openDirIcon);

		
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

		mi.addActionListener(dirListener);

		menu.add(mi);
		menu.addSeparator();

		if(dir == null && !(view.getBuffer().getVFS() instanceof FileVFS))
		{
			mi = new JMenuItem(jEdit.getProperty(
				"directory.not-local"));
			mi.setEnabled(false);
			menu.add(mi);
			return;
		}

		File directory = new File(path);

		JMenu current = menu;

		
		String backupPrefix = jEdit.getProperty("backup.prefix");
		String backupSuffix = jEdit.getProperty("backup.suffix");

		File[] list = directory.listFiles();
		if(list == null || list.length == 0)
		{
			mi = new JMenuItem(jEdit.getProperty(
				"directory.no-files"));
			mi.setEnabled(false);
			menu.add(mi);
		}
		else
		{
			int maxItems = jEdit.getIntegerProperty("menu.spillover",20);

			Arrays.sort(list,
				new StandardUtilities.StringCompare<File>(true));
			for(int i = 0; i < list.length; i++)
			{
				File file = list[i];

				String name = file.getName();

				
				if(name.endsWith(".marks"))
					continue;

				
				if(name.startsWith("#") && name.endsWith("#"))
					continue;

				
				if((backupPrefix.length() != 0
					&& name.startsWith(backupPrefix))
					|| (backupSuffix.length() != 0
					&& name.endsWith(backupSuffix)))
					continue;

				
				
				

				mi = new JMenuItem(name);
				mi.setActionCommand(file.getPath());
				mi.addActionListener(file.isDirectory()
					? dirListener
					: fileListener);
				mi.setIcon(file.isDirectory()
					? FileCellRenderer.dirIcon
					: FileCellRenderer.fileIcon);

				if(current.getItemCount() >= maxItems && i != list.length - 1)
				{
					
					JMenu newCurrent = new JMenu(
						jEdit.getProperty(
						"common.more"));
					current.add(newCurrent);
					current = newCurrent;
				}
				current.add(mi);
			}
		}
	} 

	
	private final String dir;
	
}
