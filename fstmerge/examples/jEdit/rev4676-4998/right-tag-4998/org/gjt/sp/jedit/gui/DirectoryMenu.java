

package org.gjt.sp.jedit.gui;


import javax.swing.event.*;
import javax.swing.*;
import java.awt.event.*;
import java.io.File;
import org.gjt.sp.jedit.browser.*;
import org.gjt.sp.jedit.io.FileVFS;
import org.gjt.sp.jedit.*;


public class DirectoryMenu extends EnhancedMenu
{
	
	public DirectoryMenu(String name, String dir)
	{
		super(name);
		this.dir = dir;
	} 

	
	public void menuSelected(MenuEvent evt)
	{
		super.menuSelected(evt);

		final View view = GUIUtilities.getView(this);

		if(getMenuComponentCount() != 0)
			removeAll();

		final String path;
		if(dir == null)
		{
			path = view.getBuffer().getDirectory();
		}
		else
			path = dir;

		JMenuItem mi = new JMenuItem(path + ":");
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

		add(mi);
		addSeparator();

		if(dir == null && !(view.getBuffer().getVFS() instanceof FileVFS))
		{
			mi = new JMenuItem(jEdit.getProperty(
				"directory.not-local"));
			mi.setEnabled(false);
			add(mi);
			return;
		}

		File directory = new File(path);

		JMenu current = this;

		
		String backupPrefix = jEdit.getProperty("backup.prefix");
		String backupSuffix = jEdit.getProperty("backup.suffix");

		File[] list = directory.listFiles();
		if(list == null || list.length == 0)
		{
			mi = new JMenuItem(jEdit.getProperty(
				"directory.no-files"));
			mi.setEnabled(false);
			add(mi);
		}
		else
		{
			int maxItems = jEdit.getIntegerProperty("menu.spillover",20);

			MiscUtilities.quicksort(list,
				new MiscUtilities.StringICaseCompare());
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

	public void menuDeselected(MenuEvent e) {}

	public void menuCanceled(MenuEvent e) {}

	
	private String dir;
	
}
