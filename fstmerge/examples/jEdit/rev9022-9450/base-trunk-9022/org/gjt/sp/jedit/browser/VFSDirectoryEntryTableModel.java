

package org.gjt.sp.jedit.browser;

import javax.swing.table.*;
import java.util.*;
import org.gjt.sp.jedit.io.FileVFS;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.io.VFSFile;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;


public class VFSDirectoryEntryTableModel extends AbstractTableModel
{
	
	public VFSDirectoryEntryTableModel()
	{
		extAttrs = new ArrayList<ExtendedAttribute>();
		sortColumn = 0;
		ascending = true;
	} 

	
	public void setRoot(VFS vfs, List<VFSFile> list)
	{
		extAttrs.clear();
		addExtendedAttributes(vfs);

		

		files = new Entry[list.size()];
		for(int i = 0; i < files.length; i++)
		{
			files[i] = new Entry(list.get(i),0);
		}

		

		Arrays.sort(files, new EntryCompare(getSortAttribute(sortColumn), ascending));
		fireTableStructureChanged();
	} 

	
	public int expand(VFS vfs, Entry entry, List<VFSFile> list)
	{
		int startIndex = -1;
		for(int i = 0; i < files.length; i++)
		{
			if(files[i] == entry)
				startIndex = i;
		}
		if (startIndex != -1)
			collapse(vfs,startIndex);

		addExtendedAttributes(vfs);
		entry.expanded = true;

		if(list != null)
		{
			
			Entry[] newFiles = new Entry[files.length + list.size()];
			Entry[] subdirFiles = new Entry[list.size()];

			for(int i = 0; i < list.size(); i++)
			{
				subdirFiles[i] = new Entry(
					list.get(i),entry.level + 1,entry);
			}

			
			Arrays.sort(subdirFiles, new EntryCompare(
				getSortAttribute(sortColumn), ascending));
			
			
			int nextIndex = startIndex + 1;
			System.arraycopy(files,0,newFiles,0,nextIndex);
			System.arraycopy(subdirFiles,0,newFiles,nextIndex,list.size());
			System.arraycopy(files,nextIndex,newFiles,nextIndex + list.size(),
				files.length - nextIndex);

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

			lastIndex++;

			if(e.expanded)
			{
				removeExtendedAttributes(VFSManager.getVFSForPath(
					e.dirEntry.getPath()));
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
			return jEdit.getProperty("vfs.browser." + getExtendedAttribute(col));
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

	
	public boolean getAscending()
	{
		return ascending;
	} 

	
	public int getSortColumn()
	{
		return sortColumn;
	} 

	
	public String getSortAttribute(int column)
	{
		return (column == 0) ? "name" : getExtendedAttribute(column);
	} 

	
	public boolean sortByColumn(int column)
	{
		
		ascending = (sortColumn == column) ? !ascending : true;

		
		String sortBy = getSortAttribute(column);
		if(sortBy == VFS.EA_STATUS)
			return false;

		Arrays.sort(files, new EntryCompare(sortBy, ascending));

		
		sortColumn = column;
		fireTableStructureChanged();

		return true;
	} 

	
	public String getExtendedAttribute(int index)
	{
		return extAttrs.get(index - 1).name;
	} 

	
	
	public int getColumnWidth(int i)
	{
		String extAttr = getExtendedAttribute(i);
		return jEdit.getIntegerProperty("vfs.browser."
			+ extAttr + ".width",100);
	} 
	
	
	
	public void setColumnWidth(int i, int w)
	{
		String extAttr = getExtendedAttribute(i);
		jEdit.setIntegerProperty("vfs.browser."
			+ extAttr + ".width",w);
	} 
	
	
	public VFSFile[] getFiles()
	{
		VFSFile[] f = new VFSFile[files.length];
		for(int i = 0; i < f.length; i++)
			f[i] = files[i].dirEntry;
		return f;
	} 
	
	
	Entry[] files;
	

	
	private List<ExtendedAttribute> extAttrs;
	private int sortColumn;
	private boolean ascending;

	
	private void addExtendedAttributes(VFS vfs)
	{
		String[] attrs = vfs.getExtendedAttributes();
vfs_attr_loop:	for(int i = 0; i < attrs.length; i++)
		{
			for (ExtendedAttribute attr : extAttrs)
			{
				if (attrs[i].equals(attr.name))
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
			Iterator<ExtendedAttribute> iter = extAttrs.iterator();
			while(iter.hasNext())
			{
				ExtendedAttribute attr = iter.next();
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
		VFSFile dirEntry;
		
		boolean expanded;
		
		int level;
		
		Entry parent;
		
		String extension;

		Entry(VFSFile dirEntry, int level, Entry parent)
		{
			this(dirEntry,level);
			this.parent = parent;
		}

		Entry(VFSFile dirEntry, int level)
		{
			this.dirEntry = dirEntry;
			this.level = level;
			this.extension = MiscUtilities.getFileExtension(dirEntry.getName());
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

	
	
	static class EntryCompare implements Comparator
	{
		private boolean sortIgnoreCase, sortMixFilesAndDirs, sortAscending;
		private String sortAttribute;
		
		EntryCompare(String sortBy, boolean ascending)
		{
			this.sortMixFilesAndDirs = jEdit.getBooleanProperty(
				"vfs.browser.sortMixFilesAndDirs");
			this.sortIgnoreCase = jEdit.getBooleanProperty(
				"vfs.browser.sortIgnoreCase");
			this.sortAscending = ascending;
			this.sortAttribute = sortBy;
		}

		public int compare(Object obj1, Object obj2)
		{
			Entry entry1 = (Entry)obj1;
			Entry entry2 = (Entry)obj2;

			
			if(entry1.level < entry2.level) 
				return compare(entry1, entry2.parent);
			if(entry1.level > entry2.level)
				return compare(entry1.parent, entry2);

			
			if(entry1.parent != entry2.parent)
				return compare(entry1.parent, entry2.parent);

			
			

			VFSFile file1 = entry1.dirEntry;
			VFSFile file2 = entry2.dirEntry;

			if(!sortMixFilesAndDirs)
			{
				if(file1.getType() != file2.getType())
					return file2.getType() - file1.getType();
			}

			int result;

			
			if(sortAttribute == VFS.EA_MODIFIED)
				result = (
					(Long)((FileVFS.LocalFile)file1).getModified())
					.compareTo(
					(Long)((FileVFS.LocalFile)file2).getModified());
			
			else if(sortAttribute == VFS.EA_SIZE)
				result = (
					(Long)file1.getLength())
					.compareTo(
					(Long)file2.getLength());
			
			else if(sortAttribute == VFS.EA_TYPE)
				result = StandardUtilities.compareStrings(
					entry1.extension,
					entry2.extension,
					sortIgnoreCase);
			
			else
				result = StandardUtilities.compareStrings(
					file1.getName(),
					file2.getName(),
					sortIgnoreCase);
			return (sortAscending) ? result : -result;
		}
	} 

}
