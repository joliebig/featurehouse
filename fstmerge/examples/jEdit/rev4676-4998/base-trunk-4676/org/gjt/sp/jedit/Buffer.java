

package org.gjt.sp.jedit;


import gnu.regexp.*;
import javax.swing.*;
import javax.swing.text.*;
import java.awt.Toolkit;
import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.util.*;
import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.msg.*;
import org.gjt.sp.jedit.search.RESearchMatcher;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.*;



public class Buffer
{
	
	
	public static final String LINESEP = "lineSeparator";

	
	public static final String BACKED_UP = "Buffer__backedUp";

	
	public static final String CARET = "Buffer__caret";
	public static final String SELECTION = "Buffer__selection";

	
	public static final String SCROLL_VERT = "Buffer__scrollVert";
	public static final String SCROLL_HORIZ = "Buffer__scrollHoriz";

	
	public static final String ENCODING = "encoding";

	
	public static final String TRAILING_EOL = "trailingEOL";

	
	public static final String GZIPPED = "gzipped";
	

	

	
	
	public void showInsertFileDialog(View view)
	{
		String[] files = GUIUtilities.showVFSFileDialog(view,null,
			VFSBrowser.OPEN_DIALOG,false);

		if(files != null)
			insertFile(view,files[0]);
	} 

	
	
	public void reload(View view)
	{
		if(getFlag(DIRTY))
		{
			String[] args = { name };
			int result = GUIUtilities.confirm(view,"changedreload",
				args,JOptionPane.YES_NO_OPTION,
				JOptionPane.WARNING_MESSAGE);
			if(result != JOptionPane.YES_OPTION)
				return;
		}

		view.getEditPane().saveCaretInfo();
		load(view,true);
	} 

	
	
	public boolean load(final View view, final boolean reload)
	{
		if(isPerformingIO())
		{
			GUIUtilities.error(view,"buffer-multiple-io",null);
			return false;
		}

		setBooleanProperty(BufferIORequest.ERROR_OCCURRED,false);

		setFlag(LOADING,true);

		
		
		
		if(!getFlag(TEMPORARY))
			EditBus.send(new BufferUpdate(this,view,BufferUpdate.LOAD_STARTED));

		final boolean loadAutosave;

		if(reload || !getFlag(NEW_FILE))
		{
			if(file != null)
				modTime = file.lastModified();

			
			if(!reload && autosaveFile != null && autosaveFile.exists())
				loadAutosave = recoverAutosave(view);
			else
			{
				if(autosaveFile != null)
					autosaveFile.delete();
				loadAutosave = false;
			}

			if(!loadAutosave)
			{
				VFS vfs = VFSManager.getVFSForPath(path);

				if(!checkFileForLoad(view,vfs,path))
				{
					setFlag(LOADING,false);
					return false;
				}

				if(isNewFile())
					;
				else
				{
					if(!vfs.load(view,this,path))
					{
						setFlag(LOADING,false);
						return false;
					}
				}
			}
		}
		else
			loadAutosave = false;

		
		Runnable runnable = new Runnable()
		{
			public void run()
			{
				String newPath = getStringProperty(
					BufferIORequest.NEW_PATH);
				Segment seg = (Segment)getProperty(
					BufferIORequest.LOAD_DATA);
				IntegerArray endOffsets = (IntegerArray)
					getProperty(BufferIORequest.END_OFFSETS);

				
				
				boolean readOnly = isReadOnly();
				setFlag(READ_ONLY,false);

				
				remove(0,getLength());

				if(seg == null)
					seg = new Segment(new char[1024],0,0);
				if(endOffsets == null)
				{
					endOffsets = new IntegerArray();
					endOffsets.add(1);
				}

				try
				{
					writeLock();

					int lines = offsetMgr.getLineCount();
					if(lines != 0)
					{
						firePreContentRemoved(0,0,lines,
							contentMgr.getLength());
						fireContentRemoved(0,0,lines,
							contentMgr.getLength());
					}

					
					
					
					contentMgr._setContent(seg.array,seg.count);

					offsetMgr._contentInserted(endOffsets);

					fireContentInserted(0,0,
						endOffsets.getSize(),
						seg.count);
				}
				catch(OutOfMemoryError oom)
				{
					Log.log(Log.ERROR,Buffer.this,oom);
					VFSManager.error(view,path,"out-of-memory-error",null);
				}
				finally
				{
					writeUnlock();
				}

				setFlag(READ_ONLY,readOnly);

				unsetProperty(BufferIORequest.LOAD_DATA);
				unsetProperty(BufferIORequest.END_OFFSETS);
				unsetProperty(BufferIORequest.NEW_PATH);

				undoMgr.clear();
				undoMgr.setLimit(jEdit.getIntegerProperty(
					"buffer.undoCount",100));

				if(!getFlag(TEMPORARY))
					finishLoading();

				
				fireContentInserted(0,0,getLineCount(),getLength() - 1);

				setFlag(LOADING,false);

				
				if(reload)
					setDirty(false);

				if(!loadAutosave && newPath != null)
					setPath(newPath);

				
				

				
				
				
				
				
				
				if(loadAutosave)
					setFlag(DIRTY,true);

				
				if(!getFlag(TEMPORARY))
				{
					EditBus.send(new BufferUpdate(Buffer.this,
						view,BufferUpdate.LOADED));
					
					
				}
			}
		}; 

		if(getFlag(TEMPORARY))
			runnable.run();
		else
			VFSManager.runInAWTThread(runnable);

		return true;
	} 

	
	
	public boolean insertFile(final View view, String path)
	{
		if(isPerformingIO())
		{
			GUIUtilities.error(view,"buffer-multiple-io",null);
			return false;
		}

		setBooleanProperty(BufferIORequest.ERROR_OCCURRED,false);

		path = MiscUtilities.constructPath(this.path,path);

		Buffer buffer = jEdit.getBuffer(path);
		if(buffer != null)
		{
			view.getTextArea().setSelectedText(
				buffer.getText(0,buffer.getLength()));
			return true;
		}

		VFS vfs = VFSManager.getVFSForPath(path);

		setFlag(IO,true);

		
		
		
		if(!vfs.insert(view,this,path))
		{
			setFlag(IO,false);
			return false;
		}

		
		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				setFlag(IO,false);

				SegmentBuffer sbuf = (SegmentBuffer)getProperty(
					BufferIORequest.LOAD_DATA);
				if(sbuf != null)
				{
					unsetProperty(BufferIORequest.LOAD_DATA);

					view.getTextArea().setSelectedText(sbuf.toString());
				}
			}
		});

		return true;
	} 

	
	
	public void autosave()
	{
		if(autosaveFile == null || !getFlag(AUTOSAVE_DIRTY)
			|| !getFlag(DIRTY)
			|| getFlag(LOADING)
			|| getFlag(IO))
			return;

		setFlag(AUTOSAVE_DIRTY,false);

		VFSManager.runInWorkThread(new BufferIORequest(
			BufferIORequest.AUTOSAVE,null,this,null,
			VFSManager.getFileVFS(),autosaveFile.getPath()));
	} 

	
	
	public boolean saveAs(View view, boolean rename)
	{
		String[] files = GUIUtilities.showVFSFileDialog(view,path,
			VFSBrowser.SAVE_DIALOG,false);

		
		
		if(files == null)
			return false;

		return save(view,files[0],rename);
	} 

	
	
	public boolean save(View view, String path)
	{
		return save(view,path,true);
	} 

	
	
	public boolean save(final View view, String path, final boolean rename)
	{
		if(isPerformingIO())
		{
			GUIUtilities.error(view,"buffer-multiple-io",null);
			return false;
		}

		setBooleanProperty(BufferIORequest.ERROR_OCCURRED,false);

		if(path == null && getFlag(NEW_FILE))
			return saveAs(view,rename);

		if(path == null && file != null)
		{
			long newModTime = file.lastModified();

			if(newModTime != modTime
				&& jEdit.getBooleanProperty("view.checkModStatus"))
			{
				Object[] args = { this.path };
				int result = GUIUtilities.confirm(view,
					"filechanged-save",args,
					JOptionPane.YES_NO_OPTION,
					JOptionPane.WARNING_MESSAGE);
				if(result != JOptionPane.YES_OPTION)
					return false;
			}
		}

		setFlag(IO,true);
		EditBus.send(new BufferUpdate(this,view,BufferUpdate.SAVING));

		final String oldPath = this.path;
		final String newPath = (path == null ? this.path : path);

		VFS vfs = VFSManager.getVFSForPath(newPath);

		if(!checkFileForSave(view,vfs,newPath))
		{
			setFlag(IO,false);
			return false;
		}

		if(!vfs.save(view,this,newPath))
		{
			setFlag(IO,false);
			return false;
		}

		
		VFSManager.runInAWTThread(new Runnable()
		{
			public void run()
			{
				setFlag(IO,false);
				finishSaving(view,oldPath,newPath,rename,
					getBooleanProperty(BufferIORequest
					.ERROR_OCCURRED));
			}
		});

		return true;
	} 

	
	public static final int FILE_NOT_CHANGED = 0;
	public static final int FILE_CHANGED = 1;
	public static final int FILE_DELETED = 2;
	
	public int checkFileStatus(View view)
	{
		
		
		
		
		if(!getFlag(IO) && !getFlag(LOADING) && file != null
			&& !getFlag(NEW_FILE))
		{
			boolean newReadOnly = (file.exists() && !file.canWrite());
			if(newReadOnly != getFlag(READ_ONLY))
			{
				setFlag(READ_ONLY,newReadOnly);
				EditBus.send(new BufferUpdate(this,null,
					BufferUpdate.DIRTY_CHANGED));
			}

			long oldModTime = modTime;
			long newModTime = file.lastModified();

			if(newModTime != oldModTime)
			{
				modTime = newModTime;

				if(!file.exists())
				{
					setFlag(NEW_FILE,true);
					setDirty(true);
					return FILE_DELETED;
				}
				else
				{
					return FILE_CHANGED;
				}
			}
		}

		return FILE_NOT_CHANGED;
	} 

	

	

	
	
	public long getLastModified()
	{
		return modTime;
	} 

	
	
	public void setLastModified(long modTime)
	{
		this.modTime = modTime;
	} 

	
	
	public VFS getVFS()
	{
		return VFSManager.getVFSForPath(path);
	} 

	
	
	public final File getAutosaveFile()
	{
		return autosaveFile;
	} 

	
	
	public final String getName()
	{
		return name;
	} 

	
	
	public final String getPath()
	{
		return path;
	} 

	
	
	public final String getSymlinkPath()
	{
		return symlinkPath;
	} 

	
	
	public String getDirectory()
	{
		return directory;
	} 

	
	
	public final boolean isClosed()
	{
		return getFlag(CLOSED);
	} 

	
	
	public final boolean isLoaded()
	{
		return !getFlag(LOADING);
	} 

	
	
	public final boolean isPerformingIO()
	{
		return getFlag(LOADING) || getFlag(IO);
	} 

	
	
	public final boolean isNewFile()
	{
		return getFlag(NEW_FILE);
	} 

	
	
	public final void setNewFile(boolean newFile)
	{
		setFlag(NEW_FILE,newFile);
		if(!newFile)
			setFlag(UNTITLED,false);
	} 

	
	
	public final boolean isUntitled()
	{
		return getFlag(UNTITLED);
	} 

	
	
	public final boolean isDirty()
	{
		return getFlag(DIRTY);
	} 

	
	
	public final boolean isReadOnly()
	{
		return getFlag(READ_ONLY);
	} 

	
	
	public final boolean isEditable()
	{
		return !(getFlag(READ_ONLY) || getFlag(IO) || getFlag(LOADING));
	} 

	
	
	public final void setReadOnly(boolean readOnly)
	{
		setFlag(READ_ONLY,readOnly);
	} 

	
	
	public void setDirty(boolean d)
	{
		boolean old_d = getFlag(DIRTY);

		if(d)
		{
			if(getFlag(LOADING) || getFlag(READ_ONLY))
				return;
			if(getFlag(DIRTY) && getFlag(AUTOSAVE_DIRTY))
				return;
			setFlag(DIRTY,true);
			setFlag(AUTOSAVE_DIRTY,true);
		}
		else
		{
			setFlag(DIRTY,false);
			setFlag(AUTOSAVE_DIRTY,false);

			if(autosaveFile != null)
				autosaveFile.delete();

			
			
			if(!getFlag(UNDO_IN_PROGRESS))
			{
				
				
				undoMgr.bufferSaved();
			}
		}

		if(d != old_d)
		{
			EditBus.send(new BufferUpdate(this,null,
				BufferUpdate.DIRTY_CHANGED));
		}
	} 

	
	
	public boolean isTemporary()
	{
		return getFlag(TEMPORARY);
	} 

	
	
	public Icon getIcon()
	{
		if(getFlag(DIRTY))
			return GUIUtilities.DIRTY_BUFFER_ICON;
		else if(getFlag(READ_ONLY))
			return GUIUtilities.READ_ONLY_BUFFER_ICON;
		else if(getFlag(NEW_FILE))
			return GUIUtilities.NEW_BUFFER_ICON;
		else
			return GUIUtilities.NORMAL_BUFFER_ICON;
	} 

	

	

	
	
	public final void readLock()
	{
		lock.readLock();
	} 

	
	
	public final void readUnlock()
	{
		lock.readUnlock();
	} 

	
	
	public final void writeLock()
	{
		lock.writeLock();
	} 

	
	
	public final void writeUnlock()
	{
		lock.writeUnlock();
	} 

	

	

	
	
	public int getLength()
	{
		
		return contentMgr.getLength();
	} 

	
	
	public int getLineCount()
	{
		
		return offsetMgr.getLineCount();
	} 

	
	
	public final int getLineOfOffset(int offset)
	{
		try
		{
			readLock();

			if(offset < 0 || offset > getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			return offsetMgr.getLineOfOffset(offset);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getLineStartOffset(int line)
	{
		try
		{
			readLock();

			if(line < 0 || line >= offsetMgr.getLineCount())
				throw new ArrayIndexOutOfBoundsException(line);
			else if(line == 0)
				return 0;

			return offsetMgr.getLineEndOffset(line - 1);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getLineEndOffset(int line)
	{
		try
		{
			readLock();

			if(line < 0 || line >= offsetMgr.getLineCount())
				throw new ArrayIndexOutOfBoundsException(line);

			return offsetMgr.getLineEndOffset(line);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getLineLength(int line)
	{
		try
		{
			readLock();

			return getLineEndOffset(line)
				- getLineStartOffset(line) - 1;
		}
		finally
		{
			readUnlock();
		}
	} 

	

	

	
	
	public String getLineText(int lineIndex)
	{
		try
		{
			readLock();

			return getText(getLineStartOffset(lineIndex),
				getLineLength(lineIndex));
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public void getLineText(int lineIndex, Segment segment)
	{
		try
		{
			readLock();

			getText(getLineStartOffset(lineIndex),
				getLineLength(lineIndex),segment);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public String getText(int start, int length)
	{
		try
		{
			readLock();

			if(start < 0 || length < 0
				|| start + length > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(start + ":" + length);

			return contentMgr.getText(start,length);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public void getText(int start, int length, Segment seg)
	{
		try
		{
			readLock();

			if(start < 0 || length < 0
				|| start + length > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(start + ":" + length);

			contentMgr.getText(start,length,seg);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public void insert(int offset, String str)
	{
		if(str == null)
			return;

		int len = str.length();

		if(len == 0)
			return;

		if(isReadOnly())
			throw new RuntimeException("buffer read-only");

		try
		{
			writeLock();

			if(offset < 0 || offset > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			contentMgr.insert(offset,str);

			integerArray.clear();

			for(int i = 0; i < len; i++)
			{
				if(str.charAt(i) == '\n')
					integerArray.add(i + 1);
			}

			if(!getFlag(UNDO_IN_PROGRESS))
			{
				undoMgr.contentInserted(offset,len,str,
					!getFlag(DIRTY));
			}

			contentInserted(offset,len,integerArray);
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	
	public void insert(int offset, Segment seg)
	{
		if(seg.count == 0)
			return;

		if(isReadOnly())
			throw new RuntimeException("buffer read-only");

		try
		{
			writeLock();

			if(offset < 0 || offset > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			contentMgr.insert(offset,seg);

			integerArray.clear();

			for(int i = 0; i < seg.count; i++)
			{
				if(seg.array[seg.offset + i] == '\n')
					integerArray.add(i + 1);
			}

			if(!getFlag(UNDO_IN_PROGRESS))
			{
				undoMgr.contentInserted(offset,seg.count,
					seg.toString(),!getFlag(DIRTY));
			}

			contentInserted(offset,seg.count,integerArray);
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	
	public void remove(int offset, int length)
	{
		if(length == 0)
			return;

		if(isReadOnly())
			throw new RuntimeException("buffer read-only");

		try
		{
			writeLock();

			if(offset < 0 || length < 0
				|| offset + length > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset + ":" + length);

			int startLine = offsetMgr.getLineOfOffset(offset);

			contentMgr.getText(offset,length,seg);
			int numLines = 0;
			for(int i = 0; i < seg.count; i++)
			{
				if(seg.array[seg.offset + i] == '\n')
					numLines++;
			}

			firePreContentRemoved(startLine,offset,numLines,length);

			if(!getFlag(UNDO_IN_PROGRESS))
			{
				undoMgr.contentRemoved(offset,length,
					seg.toString(),!getFlag(DIRTY));
			}

			contentMgr.remove(offset,length);

			offsetMgr.contentRemoved(startLine,offset,numLines,length);

			fireContentRemoved(startLine,offset,numLines,length);

			setDirty(true);
		}
		finally
		{
			writeUnlock();
		}
	} 

	

	

	
	
	public void undo(JEditTextArea textArea)
	{
		if(undoMgr == null)
			return;

		if(!isEditable())
		{
			textArea.getToolkit().beep();
			return;
		}

		try
		{
			writeLock();

			setFlag(UNDO_IN_PROGRESS,true);
			if(!undoMgr.undo(textArea))
				textArea.getToolkit().beep();

			fireTransactionComplete();
		}
		finally
		{
			setFlag(UNDO_IN_PROGRESS,false);

			writeUnlock();
		}
	} 

	
	
	public void redo(JEditTextArea textArea)
	{
		if(undoMgr == null)
			return;

		if(!isEditable())
		{
			Toolkit.getDefaultToolkit().beep();
			return;
		}

		try
		{
			writeLock();

			setFlag(UNDO_IN_PROGRESS,true);
			if(!undoMgr.redo(textArea))
				textArea.getToolkit().beep();

			fireTransactionComplete();
		}
		finally
		{
			setFlag(UNDO_IN_PROGRESS,false);

			writeUnlock();
		}
	} 

	
	
	public boolean isTransactionInProgress()
	{
		return getFlag(UNDO_IN_PROGRESS) || insideCompoundEdit();
	} 

	
	
	public void beginCompoundEdit()
	{
		
		
		

		try
		{
			writeLock();

			undoMgr.beginCompoundEdit();
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	
	public void endCompoundEdit()
	{
		
		
		

		try
		{
			writeLock();

			undoMgr.endCompoundEdit();

			if(!insideCompoundEdit())
				fireTransactionComplete();
		}
		finally
		{
			writeUnlock();
		}
	}

	
	
	public boolean insideCompoundEdit()
	{
		return undoMgr.insideCompoundEdit();
	} 

	

	

	
	
	public void addBufferChangeListener(BufferChangeListener listener)
	{
		bufferListeners.addElement(listener);
	} 

	
	
	public void removeBufferChangeListener(BufferChangeListener listener)
	{
		bufferListeners.removeElement(listener);
	} 

	
	
	public BufferChangeListener[] getBufferChangeListeners()
	{
		return (BufferChangeListener[])bufferListeners
			.toArray(new BufferChangeListener[
			bufferListeners.size()]);
	} 

	

	

	
	
	public void propertiesChanged()
	{
		String folding = getStringProperty("folding");
		FoldHandler handler = FoldHandler.getFoldHandler(folding);

		if(handler != null)
		{
			setFoldHandler(handler);
		}
		else
		{
			if (folding != null)
				Log.log(Log.WARNING, this, path + ": invalid 'folding' property: " + folding); 
			setFoldHandler(new DummyFoldHandler());
		}

		EditBus.send(new BufferUpdate(this,null,BufferUpdate.PROPERTIES_CHANGED));

		String newWrap = getStringProperty("wrap");
		if(wrap != null && !newWrap.equals(wrap))
		{
			offsetMgr.invalidateScreenLineCounts();
			if(isLoaded())
				offsetMgr.resetAnchors();
		}
		this.wrap = newWrap;
	} 

	
	
	public int getTabSize()
	{
		return getIntegerProperty("tabSize",8);
	} 

	
	
	public final int getIndentSize()
	{
		return getIntegerProperty("indentSize",8);
	} 

	
	
	public Object getProperty(Object name)
	{
		synchronized(propertyLock)
		{
			
			PropValue o = (PropValue)properties.get(name);
			if(o != null)
				return o.value;

			
			if(!(name instanceof String))
				return null;

			
			if(mode != null)
			{
				Object retVal = mode.getProperty((String)name);
				if(retVal == null)
					return null;

				properties.put(name,new PropValue(retVal,true));
				return retVal;
			}
			else
			{
				
				String value = jEdit.getProperty("buffer." + name);
				if(value == null)
					return null;

				
				Object retVal;
				try
				{
					retVal = new Integer(value);
				}
				catch(NumberFormatException nf)
				{
					retVal = value;
				}
				properties.put(name,new PropValue(retVal,true));
				return retVal;
			}
		}
	} 

	
	
	public void setProperty(String name, Object value)
	{
		if(value == null)
			properties.remove(name);
		else
		{
			PropValue test = (PropValue)properties.get(name);
			if(test == null)
				properties.put(name,new PropValue(value,false));
			else if(test.value.equals(value))
			{
				
			}
			else
			{
				test.value = value;
				test.defaultValue = false;
			}
		}
	} 

	
	
	public void unsetProperty(String name)
	{
		properties.remove(name);
	} 

	
	
	public String getStringProperty(String name)
	{
		Object obj = getProperty(name);
		if(obj != null)
			return obj.toString();
		else
			return null;
	} 

	
	
	public void setStringProperty(String name, String value)
	{
		setProperty(name,value);
	} 

	
	
	public boolean getBooleanProperty(String name)
	{
		Object obj = getProperty(name);
		if(obj instanceof Boolean)
			return ((Boolean)obj).booleanValue();
		else if("true".equals(obj) || "on".equals(obj) || "yes".equals(obj))
			return true;
		else
			return false;
	} 

	
	
	public void setBooleanProperty(String name, boolean value)
	{
		setProperty(name,value ? Boolean.TRUE : Boolean.FALSE);
	} 

	
	
	public int getIntegerProperty(String name, int defaultValue)
	{
		boolean defaultValueFlag;
		Object obj;
		PropValue value = (PropValue)properties.get(name);
		if(value != null)
		{
			obj = value.value;
			defaultValueFlag = value.defaultValue;
		}
		else
		{
			obj = getProperty(name);
			
			defaultValueFlag = true;
		}

		if(obj == null)
			return defaultValue;
		else if(obj instanceof Number)
			return ((Number)obj).intValue();
		else
		{
			try
			{
				int returnValue = Integer.parseInt(
					obj.toString().trim());
				properties.put(name,new PropValue(
					new Integer(returnValue),
					defaultValueFlag));
				return returnValue;
			}
			catch(Exception e)
			{
				return defaultValue;
			}
		}
	} 

	
	
	public void setIntegerProperty(String name, int value)
	{
		setProperty(name,new Integer(value));
	} 

	
	
	public RE getRegexpProperty(String name, int cflags,
		RESyntax syntax) throws REException
	{
		synchronized(propertyLock)
		{
			boolean defaultValueFlag;
			Object obj;
			PropValue value = (PropValue)properties.get(name);
			if(value != null)
			{
				obj = value.value;
				defaultValueFlag = value.defaultValue;
			}
			else
			{
				obj = getProperty(name);
				
				defaultValueFlag = true;
			}

			if(obj == null)
				return null;
			else if(obj instanceof RE)
				return (RE)obj;
			else
			{
				RE re = new RE(obj.toString(),cflags,syntax);
				properties.put(name,new PropValue(re,
					defaultValueFlag));
				return re;
			}
		}
	} 

	
	
	public ParserRuleSet getRuleSetAtOffset(int offset)
	{
		int line = getLineOfOffset(offset);
		offset -= getLineStartOffset(line);
		if(offset != 0)
			offset--;

		DefaultTokenHandler tokens = new DefaultTokenHandler();
		markTokens(line,tokens);
		Token token = TextUtilities.getTokenAtOffset(tokens.getTokens(),offset);
		return token.rules;
	} 

	
	
	public KeywordMap getKeywordMapAtOffset(int offset)
	{
		return getRuleSetAtOffset(offset).getKeywords();
	} 

	
	
	public String getContextSensitiveProperty(int offset, String name)
	{
		ParserRuleSet rules = getRuleSetAtOffset(offset);

		Object value = null;

		Hashtable rulesetProps = rules.getProperties();
		if(rulesetProps != null)
			value = rulesetProps.get(name);

		if(value == null)
		{
			value = jEdit.getMode(rules.getModeName())
				.getProperty(name);

			if(value == null)
				value = mode.getProperty(name);
		}

		if(value == null)
			return null;
		else
			return String.valueOf(value);
	} 

	
	static class PropValue
	{
		PropValue(Object value, boolean defaultValue)
		{
			if(value == null)
				throw new NullPointerException();
			this.value = value;
			this.defaultValue = defaultValue;
		}

		Object value;

		
		boolean defaultValue;

		
		public String toString()
		{
			return value.toString();
		}
	} 

	
	
	public void toggleWordWrap(View view)
	{
		String wrap = getStringProperty("wrap");
		if(wrap.equals("none"))
			wrap = "soft";
		else if(wrap.equals("soft"))
			wrap = "hard";
		else if(wrap.equals("hard"))
			wrap = "none";
		view.getStatus().setMessageAndClear(jEdit.getProperty(
			"view.status.wrap-changed",new String[] {
			wrap }));
		setProperty("wrap",wrap);
		propertiesChanged();
	} 

	
	
	public void toggleLineSeparator(View view)
	{
		String status = null;
		String lineSep = getStringProperty("lineSeparator");
		if("\n".equals(lineSep))
		{
			status = "windows";
			lineSep = "\r\n";
		}
		else if("\r\n".equals(lineSep))
		{
			status = "mac";
			lineSep = "\r";
		}
		else if("\r".equals(lineSep))
		{
			status = "unix";
			lineSep = "\n";
		}
		view.getStatus().setMessageAndClear(jEdit.getProperty(
			"view.status.linesep-changed",new String[] {
			jEdit.getProperty("lineSep." + status) }));
		setProperty("lineSeparator",lineSep);
		setDirty(true);
		propertiesChanged();
	} 

	

	

	
	
	public final Mode getMode()
	{
		return mode;
	} 

	
	
	public void setMode(String mode)
	{
		setMode(jEdit.getMode(mode));
	} 

	
	
	public void setMode(Mode mode)
	{
		
		if(mode == null)
			throw new NullPointerException("Mode must be non-null");

		this.mode = mode;

		textMode = "text".equals(mode.getName());

		setTokenMarker(mode.getTokenMarker());

		resetCachedProperties();
		propertiesChanged();
	} 

	
	
	public void setMode()
	{
		String userMode = getStringProperty("mode");
		if(userMode != null)
		{
			Mode m = jEdit.getMode(userMode);
			if(m != null)
			{
				setMode(m);
				return;
			}
		}

		String nogzName = name.substring(0,name.length() -
			(name.endsWith(".gz") ? 3 : 0));
		Mode[] modes = jEdit.getModes();

		String firstLine = getLineText(0);

		for(int i = 0; i < modes.length; i++)
		{
			if(modes[i].accept(nogzName,firstLine))
			{
				setMode(modes[i]);
				return;
			}
		}

		Mode defaultMode = jEdit.getMode(jEdit.getProperty("buffer.defaultMode"));
		if(defaultMode == null)
			defaultMode = jEdit.getMode("text");
		setMode(defaultMode);
	} 

	
	
	public void markTokens(int lineIndex, TokenHandler tokenHandler)
	{
		Segment seg;
		if(SwingUtilities.isEventDispatchThread())
			seg = this.seg;
		else
			seg = new Segment();

		try
		{
			writeLock();

			if(lineIndex < 0 || lineIndex >= offsetMgr.getLineCount())
				throw new ArrayIndexOutOfBoundsException(lineIndex);

			int firstInvalidLineContext = offsetMgr.getFirstInvalidLineContext();
			int start;
			if(textMode || firstInvalidLineContext == -1)
			{
				start = lineIndex;
			}
			else
			{
				start = Math.min(firstInvalidLineContext,
					lineIndex);
			}

			if(Debug.TOKEN_MARKER_DEBUG)
				Log.log(Log.DEBUG,this,"tokenize from " + start + " to " + lineIndex);
			for(int i = start; i <= lineIndex; i++)
			{
				getLineText(i,seg);

				TokenMarker.LineContext context = offsetMgr.getLineContext(i);
				ParserRule oldRule;
				ParserRuleSet oldRules;
				String oldSpanEndSubst;
				if(context == null)
				{
					
					oldRule = null;
					oldRules = null;
					oldSpanEndSubst = null;
				}
				else
				{
					oldRule = context.inRule;
					oldRules = context.rules;
					oldSpanEndSubst = (context.parent != null
						? context.parent.spanEndSubst
						: null);
				}

				TokenMarker.LineContext prevContext = (
					i == 0 ? null
					: offsetMgr.getLineContext(i - 1)
				);

				context = tokenMarker.markTokens(prevContext,
					(i == lineIndex ? tokenHandler
					: DummyTokenHandler.INSTANCE),seg);
				offsetMgr.setLineContext(i,context);

				
				
				
				if(oldRule != context.inRule)
				{
					nextLineRequested = true;
				}
				else if(oldRules != context.rules)
				{
					nextLineRequested = true;
				}
				else if(!MiscUtilities.objectsEqual(oldSpanEndSubst,
					context.spanEndSubst))
				{
					nextLineRequested = true;
				}
			}

			int lineCount = offsetMgr.getLineCount();
			if(lineCount - 1 == lineIndex)
				offsetMgr.setFirstInvalidLineContext(-1);
			else if(nextLineRequested)
				offsetMgr.setFirstInvalidLineContext(lineIndex + 1);
			else
			{
				offsetMgr.setFirstInvalidLineContext(Math.max(
					firstInvalidLineContext,lineIndex + 1));
			}
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	
	public boolean isNextLineRequested()
	{
		boolean retVal = nextLineRequested;
		nextLineRequested = false;
		return retVal;
	} 

	

	

	
	
	public void removeTrailingWhiteSpace(int[] lines)
	{
		try
		{
			beginCompoundEdit();

			for(int i = 0; i < lines.length; i++)
			{
				int pos, lineStart, lineEnd, tail;

				getLineText(lines[i],seg);

				
				if (seg.count == 0) continue;

				lineStart = seg.offset;
				lineEnd = seg.offset + seg.count - 1;

				for (pos = lineEnd; pos >= lineStart; pos--)
				{
					if (!Character.isWhitespace(seg.array[pos]))
						break;
				}

				tail = lineEnd - pos;

				
				if (tail == 0) continue;

				remove(getLineEndOffset(lines[i]) - 1 - tail,tail);
			}
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public void shiftIndentLeft(int[] lines)
	{
		int tabSize = getTabSize();
		int indentSize = getIndentSize();
		boolean noTabs = getBooleanProperty("noTabs");

		try
		{
			beginCompoundEdit();

			for(int i = 0; i < lines.length; i++)
			{
				int lineStart = getLineStartOffset(lines[i]);
				String line = getLineText(lines[i]);
				int whiteSpace = MiscUtilities
					.getLeadingWhiteSpace(line);
				if(whiteSpace == 0)
					continue;
				int whiteSpaceWidth = Math.max(0,MiscUtilities
					.getLeadingWhiteSpaceWidth(line,tabSize)
					- indentSize);
	
				insert(lineStart + whiteSpace,MiscUtilities
					.createWhiteSpace(whiteSpaceWidth,
					(noTabs ? 0 : tabSize)));
				remove(lineStart,whiteSpace);
			}

		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public void shiftIndentRight(int[] lines)
	{
		try
		{
			beginCompoundEdit();

			int tabSize = getTabSize();
			int indentSize = getIndentSize();
			boolean noTabs = getBooleanProperty("noTabs");
			for(int i = 0; i < lines.length; i++)
			{
				int lineStart = getLineStartOffset(lines[i]);
				String line = getLineText(lines[i]);
				int whiteSpace = MiscUtilities
					.getLeadingWhiteSpace(line);

				
				
				

				int whiteSpaceWidth = MiscUtilities
					.getLeadingWhiteSpaceWidth(
					line,tabSize) + indentSize;
				insert(lineStart + whiteSpace,MiscUtilities
					.createWhiteSpace(whiteSpaceWidth,
					(noTabs ? 0 : tabSize)));
				remove(lineStart,whiteSpace);
			}
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public boolean indentLine(int lineIndex, boolean canIncreaseIndent,
		boolean canDecreaseIndent)
	{
		getLineText(lineIndex,seg);

		int tabSize = getTabSize();

		int whitespaceChars = 0;
		int currentIndent = 0;
loop:		for(int i = 0; i < seg.count; i++)
		{
			char c = seg.array[seg.offset + i];
			switch(c)
			{
			case ' ':
				currentIndent++;
				whitespaceChars++;
				break;
			case '\t':
				currentIndent += (tabSize - (currentIndent
					% tabSize));
				whitespaceChars++;
				break;
			default:
				break loop;
			}
		}

		int idealIndent = getIndentForLine(lineIndex);
		if(idealIndent == -1)
			return false;

		if(!canDecreaseIndent && idealIndent <= currentIndent)
			return false;

		if(!canIncreaseIndent && idealIndent >= currentIndent)
			return false;

		
		try
		{
			beginCompoundEdit();

			int start = getLineStartOffset(lineIndex);

			remove(start,whitespaceChars);
			insert(start,MiscUtilities.createWhiteSpace(
				idealIndent,(getBooleanProperty("noTabs")
				? 0 : tabSize)));
		}
		finally
		{
			endCompoundEdit();
		}

		return true;
	} 

	
	
	public void indentLines(int start, int end)
	{
		try
		{
			beginCompoundEdit();
			for(int i = start; i <= end; i++)
				indentLine(i,true,true);
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public void indentLines(int[] lines)
	{
		try
		{
			beginCompoundEdit();
			for(int i = 0; i < lines.length; i++)
				indentLine(lines[i],true,true);
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public int getIndentForLine(int lineIndex)
	{
		final String EXPLICIT_START = "{{{";
		final String EXPLICIT_END = "}}}";

		if(lineIndex == 0)
			return -1;

		
		String openBrackets = getStringProperty("indentOpenBrackets");
		if(openBrackets == null)
			openBrackets = "";

		String closeBrackets = getStringProperty("indentCloseBrackets");
		if(closeBrackets == null)
			closeBrackets = "";

		RE indentNextLineRE;
		try
		{
			indentNextLineRE = getRegexpProperty("indentNextLine",
				RE.REG_ICASE,RESearchMatcher.RE_SYNTAX_JEDIT);
		}
		catch(REException re)
		{
			indentNextLineRE = null;
			Log.log(Log.ERROR,this,"Invalid indentNextLine regexp");
			Log.log(Log.ERROR,this,re);
		}

		RE indentNextLinesRE;
		try
		{
			indentNextLinesRE = getRegexpProperty("indentNextLines",
				RE.REG_ICASE,RESearchMatcher.RE_SYNTAX_JEDIT);
		}
		catch(REException re)
		{
			indentNextLinesRE = null;
			Log.log(Log.ERROR,this,"Invalid indentNextLines regexp");
			Log.log(Log.ERROR,this,re);
		}

		boolean doubleBracketIndent = getBooleanProperty("doubleBracketIndent");
		boolean lineUpClosingBracket = getBooleanProperty("lineUpClosingBracket");

		int tabSize = getTabSize();
		int indentSize = getIndentSize();
		

		
		int prevLineIndex = getPriorNonEmptyLine(lineIndex);
		if(prevLineIndex == -1)
			return -1;

		String prevLine = getLineText(prevLineIndex);

		
		boolean prevLineStart = true; 
		int indent = 0; 
		int prevLineBrackets = 0; 
		int prevLineCloseBracketIndex = -1; 
		                                    
		                                    
		                                    
		                                    
		                                    
		                                    
		                                    

		for(int i = 0; i < prevLine.length(); i++)
		{
			char c = prevLine.charAt(i);
			switch(c)
			{
			case ' ':
				if(prevLineStart)
					indent++;
				break;
			case '\t':
				if(prevLineStart)
				{
					indent += (tabSize
						- (indent
						% tabSize));
				}
				break;
			default:
				prevLineStart = false;

				if(closeBrackets.indexOf(c) != -1)
				{
					if(prevLine.regionMatches(false,
						i,EXPLICIT_END,0,3))
						i += 2;
					else
					{
						prevLineBrackets--;
						if(prevLineBrackets < 0)
						{
							if(lineUpClosingBracket)
								prevLineBrackets = 0;
							prevLineCloseBracketIndex = i;
						}
					}
				}
				else if(openBrackets.indexOf(c) != -1)
				{
					if(prevLine.regionMatches(false,
						i,EXPLICIT_START,0,3))
						i += 2;
					else
						prevLineBrackets++;
				}

				break;
			}
		}

		String line = getLineText(lineIndex);
		

		
		
		int lineBrackets = 0; 
		int closeBracketIndex = -1; 
			
		for(int i = 0; i < line.length(); i++)
		{
			char c = line.charAt(i);
			if(closeBrackets.indexOf(c) != -1)
			{
				if(line.regionMatches(false,
					i,EXPLICIT_END,0,3))
					i += 2;
				else
				{
					closeBracketIndex = i;
					lineBrackets--;
				}
			}
			else if(openBrackets.indexOf(c) != -1)
			{
				if(line.regionMatches(false,
					i,EXPLICIT_START,0,3))
					i += 2;
				else if(lineBrackets >= 0)
					lineBrackets++;
			}
		} 

		
		if(prevLineBrackets > 0)
			indent += (indentSize * prevLineBrackets);

		if(lineUpClosingBracket)
		{
			if(lineBrackets < 0)
			{
				int offset = TextUtilities.findMatchingBracket(
					this,lineIndex,closeBracketIndex);
				if(offset != -1)
				{
					String closeLine = getLineText(getLineOfOffset(offset));
					indent = MiscUtilities.getLeadingWhiteSpaceWidth(
						closeLine,tabSize);
				}
				else
					return -1;
			}
		}
		else
		{
			if(prevLineBrackets < 0)
			{
				int offset = TextUtilities.findMatchingBracket(
					this,prevLineIndex,prevLineCloseBracketIndex);
				if(offset != -1)
				{
					String closeLine = getLineText(getLineOfOffset(offset));
					indent = MiscUtilities.getLeadingWhiteSpaceWidth(
						closeLine,tabSize);
				}
				else
					return -1;
			}
		}

		
		if(lineBrackets == 0 || (doubleBracketIndent && lineBrackets > 0))
		{
			
			
			if(indentNextLinesRE != null && indentNextLinesRE.isMatch(prevLine))
				indent += indentSize;
			else if(indentNextLineRE != null)
			{
				if(indentNextLineRE.isMatch(prevLine))
					indent += indentSize;

				
				
				
				
				else if(prevLineBrackets == 0)
				{
					
					
					
					
					
					
					int prevPrevLineIndex;
					
						prevPrevLineIndex = getPriorNonEmptyLine(prevLineIndex);

					while(prevPrevLineIndex != -1)
					{
						if(indentNextLineRE.isMatch(getLineText(prevPrevLineIndex)))
							indent -= indentSize;
						else
							break;

						prevPrevLineIndex = getPriorNonEmptyLine(prevPrevLineIndex);
					}
				}
			}
		} 

		return indent;
	} 

	
	
	public int getVirtualWidth(int line, int column)
	{
		try
		{
			readLock();

			int start = getLineStartOffset(line);
			getText(start,column,seg);

			return MiscUtilities.getVirtualWidth(seg,getTabSize());
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public int getOffsetOfVirtualColumn(int line, int column,
		int[] totalVirtualWidth)
	{
		try
		{
			readLock();

			getLineText(line,seg);

			return MiscUtilities.getOffsetOfVirtualColumn(seg,
				getTabSize(),column,totalVirtualWidth);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public void insertAtColumn(int line, int col, String str)
	{
		try
		{
			writeLock();

			int[] total = new int[1];
			int offset = getOffsetOfVirtualColumn(line,col,total);
			if(offset == -1)
			{
				offset = getLineEndOffset(line) - 1;
				str = MiscUtilities.createWhiteSpace(col - total[0],0) + str;
			}
			else
				offset += getLineStartOffset(line);

			insert(offset,str);
		}
		finally
		{
			writeUnlock();
		}
	} 

	

	

	
	
	public void putProperty(Object name, Object value)
	{
		
		if(!(name instanceof String))
			return;

		setProperty((String)name,value);
	} 

	
	
	public void putBooleanProperty(String name, boolean value)
	{
		setBooleanProperty(name,value);
	} 

	
	
	public static class TokenList extends DefaultTokenHandler
	{
		public Token getFirstToken()
		{
			return getTokens();
		}
	}

	
	public TokenList markTokens(int lineIndex)
	{
		TokenList list = new TokenList();
		markTokens(lineIndex,list);
		return list;
	} 

	
	
	public Element[] getRootElements()
	{
		return new Element[] { getDefaultRootElement() };
	} 

	
	
	public Element getParagraphElement(int offset)
	{
		return new LineElement(this,getLineOfOffset(offset));
	} 

	
	
	public Element getDefaultRootElement()
	{
		return new RootElement(this);
	} 

	
	
	public void insertString(int offset, String str, AttributeSet attr)
	{
		insert(offset,str);
	} 

	
	
	public final File getFile()
	{
		return file;
	} 

	

	

	
	
	public boolean isFoldStart(int line)
	{
		return (line != getLineCount() - 1
			&& getFoldLevel(line) < getFoldLevel(line + 1));
	} 

	
	
	public void invalidateCachedFoldLevels()
	{
		offsetMgr.setFirstInvalidFoldLevel(0);
		fireFoldLevelChanged(0,getLineCount());
	} 

	
	
	public int getFoldLevel(int line)
	{
		try
		{
			writeLock();

			if(line < 0 || line >= offsetMgr.getLineCount())
				throw new ArrayIndexOutOfBoundsException(line);

			int firstInvalidFoldLevel = offsetMgr.getFirstInvalidFoldLevel();
			if(firstInvalidFoldLevel == -1 || line < firstInvalidFoldLevel
				|| foldHandler instanceof DummyFoldHandler)
			{
				return offsetMgr.getFoldLevel(line);
			}
			else
			{
				if(Debug.FOLD_DEBUG)
					Log.log(Log.DEBUG,this,"Invalid fold levels from " + firstInvalidFoldLevel + " to " + line);

				int newFoldLevel = 0;
				boolean changed = false;

				for(int i = firstInvalidFoldLevel; i <= line; i++)
				{
					newFoldLevel = foldHandler.getFoldLevel(this,i,seg);
					if(newFoldLevel != offsetMgr.getFoldLevel(i))
						changed = true;
					offsetMgr.setFoldLevel(i,newFoldLevel);
				}

				if(line == offsetMgr.getLineCount() - 1)
					offsetMgr.setFirstInvalidFoldLevel(-1);
				else
					offsetMgr.setFirstInvalidFoldLevel(line + 1);

				if(changed && !getFlag(INSIDE_INSERT))
				{
					
					fireFoldLevelChanged(firstInvalidFoldLevel,line);
				}

				return newFoldLevel;
			}
		}
		finally
		{
			writeUnlock();
		}
	} 

	
	
	public int[] getFoldAtLine(int line)
	{
		int start, end;

		if(isFoldStart(line))
		{
			start = line;
			int foldLevel = getFoldLevel(line);

			line++;

			while(getFoldLevel(line) > foldLevel)
			{
				line++;

				if(line == getLineCount())
					break;
			}

			end = line - 1;
		}
		else
		{
			start = line;
			int foldLevel = getFoldLevel(line);
			while(getFoldLevel(start) >= foldLevel)
			{
				if(start == 0)
					break;
				else
					start--;
			}

			end = line;
			while(getFoldLevel(end) >= foldLevel)
			{
				end++;

				if(end == getLineCount())
					break;
			}

			end--;
		}

		while(getLineLength(end) == 0 && end > start)
			end--;

		return new int[] { start, end };
	} 

	
	
	public FoldHandler getFoldHandler()
	{
		return foldHandler;
	} 

	

	

	
	
	public Position createPosition(int offset)
	{
		try
		{
			readLock();

			if(offset < 0 || offset > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			return offsetMgr.createPosition(offset);
		}
		finally
		{
			readUnlock();
		}
	} 

	

	

	
	
	public final Vector getMarkers()
	{
		return markers;
	} 

	
	
	public void addOrRemoveMarker(char shortcut, int pos)
	{
		int line = getLineOfOffset(pos);
		if(getMarkerAtLine(line) != null)
			removeMarker(line);
		else
			addMarker(shortcut,pos);
	} 

	
	
	public void addMarker(char shortcut, int pos)
	{
		if(!getFlag(READ_ONLY) && jEdit.getBooleanProperty("persistentMarkers"))
			setDirty(true);

		Marker markerN = new Marker(this,shortcut,pos);
		boolean added = false;

		
		if(!getFlag(LOADING))
		{
			markerN.createPosition();

			for(int i = 0; i < markers.size(); i++)
			{
				Marker marker = (Marker)markers.elementAt(i);
				if(shortcut != '\0' && marker.getShortcut() == shortcut)
					marker.setShortcut('\0');

				if(marker.getPosition() == pos)
				{
					markers.removeElementAt(i);
					i--;
				}
			}

			for(int i = 0; i < markers.size(); i++)
			{
				Marker marker = (Marker)markers.elementAt(i);
				if(marker.getPosition() > pos)
				{
					markers.insertElementAt(markerN,i);
					added = true;
					break;
				}
			}
		}

		if(!added)
			markers.addElement(markerN);

		if(!getFlag(LOADING) && !getFlag(TEMPORARY))
		{
			EditBus.send(new BufferUpdate(this,null,
				BufferUpdate.MARKERS_CHANGED));
		}
	} 

	
	
	public Marker getMarkerInRange(int start, int end)
	{
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = (Marker)markers.elementAt(i);
			int pos = marker.getPosition();
			if(pos >= start && pos < end)
				return marker;
		}

		return null;
	} 

	
	
	public Marker getMarkerAtLine(int line)
	{
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = (Marker)markers.elementAt(i);
			if(getLineOfOffset(marker.getPosition()) == line)
				return marker;
		}

		return null;
	} 

	
	
	public void removeMarker(int line)
	{
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = (Marker)markers.elementAt(i);
			if(getLineOfOffset(marker.getPosition()) == line)
			{
				if(!getFlag(READ_ONLY) && jEdit.getBooleanProperty("persistentMarkers"))
					setDirty(true);

				marker.removePosition();
				markers.removeElementAt(i);
				i--;
			}
		}

		EditBus.send(new BufferUpdate(this,null,
			BufferUpdate.MARKERS_CHANGED));
	} 

	
	
	public void removeAllMarkers()
	{
		if(!getFlag(READ_ONLY) && jEdit.getBooleanProperty("persistentMarkers"))
			setDirty(true);

		for(int i = 0; i < markers.size(); i++)
			((Marker)markers.elementAt(i)).removePosition();

		markers.removeAllElements();

		if(!getFlag(LOADING))
		{
			EditBus.send(new BufferUpdate(this,null,
				BufferUpdate.MARKERS_CHANGED));
		}
	} 

	
	
	public Marker getMarker(char shortcut)
	{
		Enumeration e = markers.elements();
		while(e.hasMoreElements())
		{
			Marker marker = (Marker)e.nextElement();
			if(marker.getShortcut() == shortcut)
				return marker;
		}
		return null;
	} 

	

	

	
	
	public void setWaitSocket(Socket waitSocket)
	{
		this.waitSocket = waitSocket;
	} 

	
	
	public final Buffer getNext()
	{
		return next;
	} 

	
	
	public final Buffer getPrev()
	{
		return prev;
	} 

	
	
	public final int getIndex()
	{
		int count = 0;
		Buffer buffer = prev;
		for(;;)
		{
			if(buffer == null)
				break;
			count++;
			buffer = buffer.prev;
		}
		return count;
	} 

	
	
	public String toString()
	{
		return name + " (" + directory + ")";
	} 

	

	

	
	
	public OffsetManager _getOffsetManager()
	{
		return offsetMgr;
	} 

	
	
	public int _displayLock()
	{
		for(int i = 0; i < OffsetManager.MAX_DISPLAY_COUNT; i++)
		{
			if((displayLock & (1 << i)) == 0)
			{
				displayLock |= (1 << i);
				return i;
			}
		}

		
		throw new InternalError("Too many text areas editing this buffer");
	} 

	
	
	public void _displayUnlock(int index)
	{
		displayLock &= ~(1 << index);
	} 

	

	
	Buffer prev;
	Buffer next;

	
	Buffer(String path, boolean newFile, boolean temp, Hashtable props)
	{
		lock = new ReadWriteLock();
		propertyLock = new Object();
		contentMgr = new ContentManager();
		offsetMgr = new OffsetManager(this);
		integerArray = new IntegerArray();
		undoMgr = new UndoManager(this);
		bufferListeners = new Vector();
		seg = new Segment();
		markers = new Vector();
		properties = new HashMap();

		
		Enumeration e = props.keys();
		while(e.hasMoreElements())
		{
			Object key = e.nextElement();
			Object value = props.get(key);

			properties.put(key,new PropValue(value,false));
		} 

		
		
		if(getProperty(ENCODING) == null)
			properties.put(ENCODING,new PropValue(System.getProperty("file.encoding"),false));
		if(getProperty(LINESEP) == null)
			properties.put(LINESEP,new PropValue(System.getProperty("line.separator"),false));

		setFlag(TEMPORARY,temp);

		
		setPath(path);

		
		setFlag(UNTITLED,newFile);
		setFlag(NEW_FILE,newFile);
	} 

	
	void commitTemporary()
	{
		setFlag(TEMPORARY,false);

		finishLoading();
	} 

	
	void resetCachedProperties()
	{
		
		
		Iterator iter = properties.values().iterator();
		while(iter.hasNext())
		{
			PropValue value = (PropValue)iter.next();
			if(value.defaultValue)
				iter.remove();
		}
	} 

	
	void close()
	{
		setFlag(CLOSED,true);

		if(autosaveFile != null)
			autosaveFile.delete();

		
		if(waitSocket != null && !waitSocket.isClosed())
		{
			try
			{
				waitSocket.getOutputStream().write('\0');
				waitSocket.getOutputStream().flush();
				waitSocket.getInputStream().close();
				waitSocket.getOutputStream().close();
				waitSocket.close();
			}
			catch(IOException io)
			{
				
			}
		}
	} 

	

	

	

	
	private void setFlag(int flag, boolean value)
	{
		if(value)
			flags |= (1 << flag);
		else
			flags &= ~(1 << flag);
	} 

	
	private boolean getFlag(int flag)
	{
		int mask = (1 << flag);
		return (flags & mask) == mask;
	} 

	
	private static final int CLOSED = 0;
	private static final int LOADING = 1;
	private static final int IO = 2;
	private static final int NEW_FILE = 3;
	private static final int UNTITLED = 4;
	private static final int AUTOSAVE_DIRTY = 5;
	private static final int DIRTY = 6;
	private static final int READ_ONLY = 7;
	private static final int UNDO_IN_PROGRESS = 8;
	private static final int TEMPORARY = 9;
	private static final int INSIDE_INSERT = 10;
	

	private int flags;

	

	
	private String path;
	private String symlinkPath;
	private String name;
	private String directory;
	private File file;
	private File autosaveFile;
	private long modTime;
	private Mode mode;
	private HashMap properties;

	private ReadWriteLock lock;
	private Object propertyLock;
	private ContentManager contentMgr;
	private OffsetManager offsetMgr;
	private IntegerArray integerArray;
	private UndoManager undoMgr;
	private Vector bufferListeners;

	private Vector markers;

	
	private boolean textMode;
	private TokenMarker tokenMarker;
	private Segment seg;
	private boolean nextLineRequested;
	private FoldHandler foldHandler;
	private int displayLock;

	
	private String wrap;

	private Socket waitSocket;
	

	
	private void setPath(String path)
	{
		this.path = path;
		this.symlinkPath = path;
		VFS vfs = VFSManager.getVFSForPath(path);
		if((vfs.getCapabilities() & VFS.WRITE_CAP) == 0)
			setReadOnly(true);
		this.name = vfs.getFileName(path);
		this.directory = vfs.getParentOfPath(path);

		if(vfs instanceof FileVFS)
		{
			file = new File(path);
			try
			{
				symlinkPath = file.getCanonicalPath();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,io);
			}

			
			
			if(autosaveFile != null)
				autosaveFile.delete();
			autosaveFile = new File(file.getParent(),'#' + name + '#');
		}
		else
		{
			
			
			file = null;
			autosaveFile = null;
		}
	} 

	
	private boolean recoverAutosave(final View view)
	{
		if(!autosaveFile.canRead())
			return false;

		
		GUIUtilities.hideSplashScreen();

		final Object[] args = { autosaveFile.getPath() };
		int result = GUIUtilities.confirm(view,"autosave-found",args,
			JOptionPane.YES_NO_OPTION,JOptionPane.WARNING_MESSAGE);

		if(result == JOptionPane.YES_OPTION)
		{
			VFSManager.getFileVFS().load(view,this,autosaveFile.getPath());

			
			
			VFSManager.runInAWTThread(new Runnable()
			{
				public void run()
				{
					GUIUtilities.message(view,"autosave-loaded",args);
				}
			});

			return true;
		}
		else
			return false;
	} 

	
	private boolean checkFileForLoad(View view, VFS vfs, String path)
	{
		if((vfs.getCapabilities() & VFS.LOW_LATENCY_CAP) != 0)
		{
			Object session = vfs.createVFSSession(path,view);
			if(session == null)
				return false;

			try
			{
				VFS.DirectoryEntry file = vfs._getDirectoryEntry(session,path,view);
				if(file == null)
				{
					setNewFile(true);
					return true;
				}

				if(!file.canRead)
				{
					VFSManager.error(view,path,"ioerror.no-read",null);
					setNewFile(false);
					return false;
				}

				setReadOnly(!file.canWrite);

				if(file.type != VFS.DirectoryEntry.FILE)
				{
					VFSManager.error(view,path,
						"ioerror.open-directory",null);
					setNewFile(false);
					return false;
				}
			}
			catch(IOException io)
			{
				VFSManager.error(view,path,"ioerror",
					new String[] { io.toString() });
				return false;
			}
			finally
			{
				try
				{
					vfs._endVFSSession(session,view);
				}
				catch(IOException io)
				{
					VFSManager.error(view,path,"ioerror",
						new String[] { io.toString() });
					return false;
				}
			}
		}

		return true;
	} 

	
	private boolean checkFileForSave(View view, VFS vfs, String path)
	{
		if((vfs.getCapabilities() & VFS.LOW_LATENCY_CAP) != 0)
		{
			Object session = vfs.createVFSSession(path,view);
			if(session == null)
				return false;

			try
			{
				VFS.DirectoryEntry file = vfs._getDirectoryEntry(session,path,view);
				if(file == null)
					return true;

				if(file.type != VFS.DirectoryEntry.FILE)
				{
					VFSManager.error(view,path,
						"ioerror.save-directory",null);
					return false;
				}
			}
			catch(IOException io)
			{
				VFSManager.error(view,path,"ioerror",
					new String[] { io.toString() });
				return false;
			}
			finally
			{
				try
				{
					vfs._endVFSSession(session,view);
				}
				catch(IOException io)
				{
					VFSManager.error(view,path,"ioerror",
						new String[] { io.toString() });
					return false;
				}
			}
		}

		return true;
	} 

	
	private void finishLoading()
	{
		parseBufferLocalProperties();
		
		
		FoldHandler oldFoldHandler = foldHandler;
		setMode();

		if(foldHandler == oldFoldHandler)
		{
			
			
			
			offsetMgr.setFirstInvalidFoldLevel(0);

			int collapseFolds = getIntegerProperty("collapseFolds",0);
			if(collapseFolds == 0)
			{
				
			}
			else
			{
				offsetMgr.expandFolds(collapseFolds);
			}
		}

		offsetMgr.resetAnchors();

		
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = (Marker)markers.elementAt(i);
			int pos = marker.getPosition();
			if(pos > getLength())
				marker.setPosition(getLength());
			marker.removePosition();
			marker.createPosition();
		}
	} 

	
	private void finishSaving(View view, String oldPath, String path,
		boolean rename, boolean error)
	{
		
		
		
		if(!error && !path.equals(oldPath))
		{
			Buffer buffer = jEdit.getBuffer(path);

			if(rename)
			{
				
				if(buffer != null && 
					!buffer.getPath().equals(oldPath))
				{
					buffer.setDirty(false);
					jEdit.closeBuffer(view,buffer);
				}

				setPath(path);
			}
			else
			{
				
				if(buffer != null && 
					!buffer.getPath().equals(oldPath))
				{
					buffer.load(view,true);
				}
			}
		} 

		
		if(rename)
		{
			if(file != null)
				modTime = file.lastModified();

			if(!error)
			{
				
				
				
				
				
				try
				{
					writeLock();

					if(autosaveFile != null)
						autosaveFile.delete();

					setFlag(AUTOSAVE_DIRTY,false);
					setFlag(READ_ONLY,false);
					setFlag(NEW_FILE,false);
					setFlag(UNTITLED,false);
					setFlag(DIRTY,false);

					
					
					
					undoMgr.bufferSaved();
				}
				finally
				{
					writeUnlock();
				}

				parseBufferLocalProperties();

				if(!getPath().equals(oldPath))
				{
					jEdit.updatePosition(Buffer.this);
					setMode();
				}
				else
				{
					
					String newMode = getStringProperty("mode");
					if(newMode != null &&
						!newMode.equals(getMode()
						.getName()))
						setMode();
					else
						propertiesChanged();
				}

				EditBus.send(new BufferUpdate(Buffer.this,
					view,BufferUpdate.DIRTY_CHANGED));

				
				EditBus.send(new BufferUpdate(Buffer.this,
					view,BufferUpdate.SAVED));
			}
		} 
	} 

	
	private void parseBufferLocalProperties()
	{
		int lastLine = Math.min(9,getLineCount() - 1);
		parseBufferLocalProperties(getText(0,getLineEndOffset(lastLine) - 1));

		
		
		int firstLine = Math.max(lastLine + 1, getLineCount() - 10);
		if(firstLine < getLineCount())
		{
			int length = getLineEndOffset(getLineCount() - 1) 
				- (getLineStartOffset(firstLine) + 1);
			parseBufferLocalProperties(getText(getLineStartOffset(firstLine),length));
		}
	} 

	
	private void parseBufferLocalProperties(String prop)
	{
		StringBuffer buf = new StringBuffer();
		String name = null;
		boolean escape = false;
		for(int i = 0; i < prop.length(); i++)
		{
			char c = prop.charAt(i);
			switch(c)
			{
			case ':':
				if(escape)
				{
					escape = false;
					buf.append(':');
					break;
				}
				if(name != null)
				{
					
					
					
					
					
					properties.put(name,new PropValue(buf.toString(),false));
					name = null;
				}
				buf.setLength(0);
				break;
			case '=':
				if(escape)
				{
					escape = false;
					buf.append('=');
					break;
				}
				name = buf.toString();
				buf.setLength(0);
				break;
			case '\\':
				if(escape)
					buf.append('\\');
				escape = !escape;
				break;
			case 'n':
				if(escape)
				{	buf.append('\n');
					escape = false;
					break;
				}
			case 'r':
				if(escape)
				{	buf.append('\r');
					escape = false;
					break;
				}
			case 't':
				if(escape)
				{
					buf.append('\t');
					escape = false;
					break;
				}
			default:
				buf.append(c);
				break;
			}
		}
	} 

	
	private void setTokenMarker(TokenMarker tokenMarker)
	{
		TokenMarker oldTokenMarker = this.tokenMarker;

		this.tokenMarker = tokenMarker;

		
		if(oldTokenMarker != null && tokenMarker != oldTokenMarker)
		{
			offsetMgr.setFirstInvalidLineContext(0);
		}
	} 

	
	private void setFoldHandler(FoldHandler foldHandler)
	{
		FoldHandler oldFoldHandler = this.foldHandler;

		if(foldHandler.equals(oldFoldHandler))
			return;

		this.foldHandler = foldHandler;

		offsetMgr.setFirstInvalidFoldLevel(0);

		int collapseFolds = getIntegerProperty("collapseFolds",0);
		offsetMgr.expandFolds(collapseFolds);

		if(isLoaded())
			offsetMgr.resetAnchors();
	} 

	
	
	private int getPriorNonEmptyLine(int lineIndex)
	{
		for(int i = lineIndex - 1; i >= 0; i--)
		{
			if(getLineLength(i) != 0)
				return i;
		}

		return -1;
	} 

	
	private void contentInserted(int offset, int length,
		IntegerArray endOffsets)
	{
		try
		{
			setFlag(INSIDE_INSERT,true);

			int startLine = offsetMgr.getLineOfOffset(offset);
			int numLines = endOffsets.getSize();

			offsetMgr.contentInserted(startLine,offset,numLines,length,
				endOffsets);

			setDirty(true);

			if(!getFlag(LOADING))
				fireContentInserted(startLine,offset,numLines,length);
		}
		finally
		{
			setFlag(INSIDE_INSERT,false);
		}
	} 

	

	
	private void fireFoldLevelChanged(int start, int end)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			try
			{
				((BufferChangeListener)bufferListeners.elementAt(i))
					.foldLevelChanged(this,start,end);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event:");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	private void fireContentInserted(int startLine, int offset,
		int numLines, int length)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			try
			{
				((BufferChangeListener)bufferListeners.elementAt(i))
					.contentInserted(this,startLine,offset,
					numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event:");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	private void fireContentRemoved(int startLine, int offset,
		int numLines, int length)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			try
			{
				((BufferChangeListener)bufferListeners.elementAt(i))
					.contentRemoved(this,startLine,offset,
					numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event:");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	private void firePreContentRemoved(int startLine, int offset,
		int numLines, int length)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			try
			{
				((BufferChangeListener)bufferListeners.elementAt(i))
					.preContentRemoved(this,startLine,offset,
					numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event:");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	private void fireTransactionComplete()
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			try
			{
				((BufferChangeListener)bufferListeners.elementAt(i))
					.transactionComplete(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event:");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	

	
}
