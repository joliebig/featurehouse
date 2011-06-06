

package org.gjt.sp.jedit.browser;

import javax.swing.table.*;
import javax.swing.*;
import java.util.*;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;


public class VFSDirectoryEntryTableModel extends AbstractTableModel
{
	
	public VFSDirectoryEntryTableModel()
	{
		extAttrs = new ArrayList();
	} 

	
	public void setRoot(VFS vfs, ArrayList list)
	{
		extAttrs.clear();
		addExtendedAttributes(vfs);

		

		files = new Entry[list.size()];
		for(int i = 0; i < files.length; i++)
		{
			files[i] = new Entry((VFS.DirectoryEntry)list.get(i),0);
		}

		

		fireTableStructureChanged();
	} 

	
	public int expand(VFS vfs, Entry entry, ArrayList list)
	{
		int startIndex = -1;
		for(int i = 0; i < files.length; i++)
		{
			if(files[i] == entry)
				startIndex = i;
		}

		collapse(vfs,startIndex);

		addExtendedAttributes(vfs);
		entry.expanded = true;

		if(list != null)
		{
			Entry[] newFiles = new Entry[files.length + list.size()];
			System.arraycopy(files,0,newFiles,0,startIndex + 1);
			for(int i = 0; i < list.size(); i++)
			{
				newFiles[startIndex + i + 1] = new Entry(
					(VFS.DirectoryEntry)list.get(i),
					entry.level + 1);
			}
			System.arraycopy(files,startIndex + 1,
				newFiles,startIndex + list.size() + 1,
				files.length - startIndex - 1);
			this.files = newFiles;

			
		}

		

		fireTableStructureChanged();

		return startIndex;
	} 

	
	public void collapse(VFS vfs, int index)
	{
		Entry entry = files[index];
		if(!entry.expanded)
			return;

		entry.expanded = false;

		int lastIndex = index + 1;
		while(lastIndex < files.length)
		{
			Entry e = files[lastIndex];

			if(e.level <= entry.level)
				break;
			else
				lastIndex++;

			if(e.expanded)
			{
				removeExtendedAttributes(VFSManager.getVFSForPath(
					e.dirEntry.path));
			}
		}

		removeExtendedAttributes(vfs);

		Entry[] newFiles = new Entry[files.length - lastIndex + index + 1];
		System.arraycopy(files,0,newFiles,0,index + 1);
		System.arraycopy(files,lastIndex,newFiles,index + 1,
			files.length - lastIndex);

		files = newFiles;

		

		fireTableStructureChanged();
	} 

	
	public int getColumnCount()
	{
		return 1 + extAttrs.size();
	} 

	
	public int getRowCount()
	{
		if(files == null)
			return 0;
		else
			return files.length;
	} 

	
	public String getColumnName(int col)
	{
		if(col == 0)
			return jEdit.getProperty("vfs.browser.name");
		else
			return jEdit.getProperty("vfs.browser." + getExtendedAttribute(col - 1));
	} 

	
	public Class getColumnClass(int col)
	{
		return Entry.class;
	} 

	
	public Object getValueAt(int row, int col)
	{
		if(files == null)
			return null;
		else
			return files[row];
	} 

	
	public String getExtendedAttribute(int index)
	{
		return ((ExtendedAttribute)extAttrs.get(index)).name;
	} 

	
	Entry[] files;
	

	
	private List extAttrs;

	
	private void addExtendedAttributes(VFS vfs)
	{
		String[] attrs = vfs.getExtendedAttributes();
vfs_attr_loop:	for(int i = 0; i < attrs.length; i++)
		{
			Iterator iter = extAttrs.iterator();
			while(iter.hasNext())
			{
				ExtendedAttribute attr = (ExtendedAttribute)
					iter.next();
				if(attrs[i].equals(attr.name))
				{
					attr.ref++;
					continue vfs_attr_loop;
				}
			}

			
			
			
			extAttrs.add(new ExtendedAttribute(attrs[i]));
		}
	} 

	
	private void removeExtendedAttributes(VFS vfs)
	{
		String[] attrs = vfs.getExtendedAttributes();
vfs_attr_loop:	for(int i = 0; i < attrs.length; i++)
		{
			Iterator iter = extAttrs.iterator();
			while(iter.hasNext())
			{
				ExtendedAttribute attr = (ExtendedAttribute)
					iter.next();
				if(attrs[i].equals(attr.name))
				{
					if(--attr.ref == 0)
					{
						
						
						
						iter.remove();
					}

					continue vfs_attr_loop;
				}
			}

			
			
			Log.log(Log.WARNING,this,"We forgot about " + attrs[i]);
		}
	} 

	

	
	static class Entry
	{
		VFS.DirectoryEntry dirEntry;
		boolean expanded;
		
		int level;

		Entry(VFS.DirectoryEntry dirEntry, int level)
		{
			this.dirEntry = dirEntry;
			this.level = level;
		}
	} 

	
	static class ExtendedAttribute
	{
		
		int ref;

		String name;

		ExtendedAttribute(String name)
		{
			this.name = name;
			ref = 1;
		}
	} 
}
