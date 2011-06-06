

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

	
	public static final String ENCODING_AUTODETECT = "encodingAutodetect";

	
	public static final String TRAILING_EOL = "trailingEOL";

	
	public static final String GZIPPED = "gzipped";
	

	

	
	
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

				
				
				if(reload || !getFlag(NEW_FILE))
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

					
					firePreContentRemoved(0,0,getLineCount()
						- 1,getLength());

					contentMgr.remove(0,getLength());
					lineMgr.contentRemoved(0,0,getLineCount()
						- 1,getLength());
					positionMgr.contentRemoved(0,getLength());
					fireContentRemoved(0,0,getLineCount()
						- 1,getLength());

					
					
					
					contentMgr._setContent(seg.array,seg.count);

					lineMgr._contentInserted(endOffsets);
					positionMgr.contentInserted(0,seg.count);

					fireContentInserted(0,0,
						endOffsets.getSize() - 1,
						seg.count - 1);
				}
				finally
				{
					writeUnlock();
				}

				unsetProperty(BufferIORequest.LOAD_DATA);
				unsetProperty(BufferIORequest.END_OFFSETS);
				unsetProperty(BufferIORequest.NEW_PATH);

				undoMgr.clear();
				undoMgr.setLimit(jEdit.getIntegerProperty(
					"buffer.undoCount",100));

				if(!getFlag(TEMPORARY))
					finishLoading();

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

		
		
		
		return vfs.insert(view,this,path);
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

		EditBus.send(new BufferUpdate(this,view,BufferUpdate.SAVING));

		setFlag(IO,true);

		final String oldPath = this.path;
		final String oldSymlinkPath = this.symlinkPath;
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
				finishSaving(view,oldPath,oldSymlinkPath,
					newPath,rename,getBooleanProperty(
					BufferIORequest.ERROR_OCCURRED));
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

	
	
	public File getAutosaveFile()
	{
		return autosaveFile;
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public String getPath()
	{
		return path;
	} 

	
	
	public String getSymlinkPath()
	{
		return symlinkPath;
	} 

	
	
	public String getDirectory()
	{
		return directory;
	} 

	
	
	public boolean isClosed()
	{
		return getFlag(CLOSED);
	} 

	
	
	public boolean isLoaded()
	{
		return !getFlag(LOADING);
	} 

	
	
	public boolean isPerformingIO()
	{
		return getFlag(LOADING) || getFlag(IO);
	} 

	
	
	public boolean isNewFile()
	{
		return getFlag(NEW_FILE);
	} 

	
	
	public void setNewFile(boolean newFile)
	{
		setFlag(NEW_FILE,newFile);
		if(!newFile)
			setFlag(UNTITLED,false);
	} 

	
	
	public boolean isUntitled()
	{
		return getFlag(UNTITLED);
	} 

	
	
	public boolean isDirty()
	{
		return getFlag(DIRTY);
	} 

	
	
	public boolean isReadOnly()
	{
		return getFlag(READ_ONLY) || getFlag(READ_ONLY_OVERRIDE);
	} 

	
	
	public boolean isEditable()
	{
		return !(getFlag(READ_ONLY) || getFlag(READ_ONLY_OVERRIDE)
			|| getFlag(IO) || getFlag(LOADING));
	} 

	
	
	public void setReadOnly(boolean readOnly)
	{
		setFlag(READ_ONLY_OVERRIDE,readOnly);
	} 

	
	
	public void setDirty(boolean d)
	{
		boolean old_d = getFlag(DIRTY);
		boolean editable = isEditable();

		if(d)
		{
			if(editable)
			{
				setFlag(DIRTY,true);
				setFlag(AUTOSAVE_DIRTY,true);
			}
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

		if(d != old_d && editable)
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
		else if(getFlag(READ_ONLY) || getFlag(READ_ONLY_OVERRIDE))
			return GUIUtilities.READ_ONLY_BUFFER_ICON;
		else if(getFlag(NEW_FILE))
			return GUIUtilities.NEW_BUFFER_ICON;
		else
			return GUIUtilities.NORMAL_BUFFER_ICON;
	} 

	

	

	
	
	public void readLock()
	{
		lock.readLock();
	} 

	
	
	public void readUnlock()
	{
		lock.readUnlock();
	} 

	
	
	public void writeLock()
	{
		lock.writeLock();
	} 

	
	
	public void writeUnlock()
	{
		lock.writeUnlock();
	} 

	

	

	
	
	public int getLength()
	{
		
		return contentMgr.getLength();
	} 

	
	
	public int getLineCount()
	{
		
		return lineMgr.getLineCount();
	} 

	
	
	public int getLineOfOffset(int offset)
	{
		try
		{
			readLock();

			if(offset < 0 || offset > getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			return lineMgr.getLineOfOffset(offset);
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

			if(line < 0 || line >= lineMgr.getLineCount())
				throw new ArrayIndexOutOfBoundsException(line);
			else if(line == 0)
				return 0;

			return lineMgr.getLineEndOffset(line - 1);
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

			if(line < 0 || line >= lineMgr.getLineCount())
				throw new ArrayIndexOutOfBoundsException(line);

			return lineMgr.getLineEndOffset(line);
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

	
	
	public void invalidateCachedScreenLineCounts()
	{
		lineMgr.invalidateScreenLineCounts();
	} 

	

	

	
	
	public String getLineText(int line)
	{
		if(line < 0 || line >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		try
		{
			readLock();

			int start = (line == 0 ? 0
				: lineMgr.getLineEndOffset(line - 1));
			int end = lineMgr.getLineEndOffset(line);

			return getText(start,end - start - 1);
		}
		finally
		{
			readUnlock();
		}
	} 

	
	
	public void getLineText(int line, Segment segment)
	{
		if(line < 0 || line >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		try
		{
			readLock();

			int start = (line == 0 ? 0
				: lineMgr.getLineEndOffset(line - 1));
			int end = lineMgr.getLineEndOffset(line);

			getText(start,end - start - 1,segment);
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
			setFlag(TRANSACTION,true);
			writeLock();

			if(offset < 0 || length < 0
				|| offset + length > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset + ":" + length);

			int startLine = lineMgr.getLineOfOffset(offset);
			int endLine = lineMgr.getLineOfOffset(offset + length);

			int numLines = endLine - startLine;

			if(!getFlag(UNDO_IN_PROGRESS) && !getFlag(LOADING))
			{
				undoMgr.contentRemoved(offset,length,
					getText(offset,length),
					!getFlag(DIRTY));
			}

			firePreContentRemoved(startLine,offset,numLines,length);

			contentMgr.remove(offset,length);
			lineMgr.contentRemoved(startLine,offset,numLines,length);
			positionMgr.contentRemoved(offset,length);

			fireContentRemoved(startLine,offset,numLines,length);

			
			if(!getFlag(UNDO_IN_PROGRESS) && !insideCompoundEdit())
				fireTransactionComplete();

			setDirty(true);
		}
		finally
		{
			setFlag(TRANSACTION,false);
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
			int caret = undoMgr.undo();

			if(caret == -1)
				textArea.getToolkit().beep();
			else
				textArea.setCaretPosition(caret);

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
			int caret = undoMgr.redo();
			if(caret == -1)
				textArea.getToolkit().beep();
			else
				textArea.setCaretPosition(caret);

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
		return getFlag(TRANSACTION)
			|| getFlag(UNDO_IN_PROGRESS)
			|| insideCompoundEdit();
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

	

	
	public static final int NORMAL_PRIORITY = 0;
	public static final int HIGH_PRIORITY = 1;
	static class Listener
	{
		BufferChangeListener listener;
		int priority;

		Listener(BufferChangeListener listener, int priority)
		{
			this.listener = listener;
			this.priority = priority;
		}
	}

	
	
	public void addBufferChangeListener(BufferChangeListener listener,
		int priority)
	{
		Listener l = new Listener(listener,priority);
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			Listener _l = (Listener)bufferListeners.get(i);
			if(_l.priority < priority)
			{
				bufferListeners.insertElementAt(l,i);
				return;
			}
		}
		bufferListeners.addElement(l);
	} 

	
	
	public void addBufferChangeListener(BufferChangeListener listener)
	{
		addBufferChangeListener(listener,NORMAL_PRIORITY);
	} 

	
	
	public void removeBufferChangeListener(BufferChangeListener listener)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			if(((Listener)bufferListeners.get(i)).listener == listener)
			{
				bufferListeners.removeElementAt(i);
				return;
			}
		}
	} 

	
	
	public BufferChangeListener[] getBufferChangeListeners()
	{
		BufferChangeListener[] returnValue
			= new BufferChangeListener[
			bufferListeners.size()];
		for(int i = 0; i < returnValue.length; i++)
		{
			returnValue[i] = ((Listener)bufferListeners.get(i))
				.listener;
		}
		return returnValue;
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
	} 

	
	
	public int getTabSize()
	{
		int tabSize = getIntegerProperty("tabSize",8);
		if(tabSize <= 0)
			return 8;
		else
			return tabSize;
	} 

	
	
	public int getIndentSize()
	{
		int indentSize = getIntegerProperty("indentSize",8);
		if(indentSize <= 0)
			return 8;
		else
			return indentSize;
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

	

	

	
	
	public Mode getMode()
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

		
		
		for(int i = modes.length - 1; i >= 0; i--)
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

		if(lineIndex < 0 || lineIndex >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(lineIndex);

		int firstInvalidLineContext = lineMgr.getFirstInvalidLineContext();
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
		TokenMarker.LineContext oldContext = null;
		TokenMarker.LineContext context = null;
		for(int i = start; i <= lineIndex; i++)
		{
			getLineText(i,seg);

			oldContext = lineMgr.getLineContext(i);

			TokenMarker.LineContext prevContext = (
				(i == 0 || textMode) ? null
				: lineMgr.getLineContext(i - 1)
			);

			context = tokenMarker.markTokens(prevContext,
				(i == lineIndex ? tokenHandler
				: DummyTokenHandler.INSTANCE),seg);
			lineMgr.setLineContext(i,context);
		}

		int lineCount = lineMgr.getLineCount();
		if(lineCount - 1 == lineIndex)
			lineMgr.setFirstInvalidLineContext(-1);
		else if(oldContext != context)
			lineMgr.setFirstInvalidLineContext(lineIndex + 1);
		else if(firstInvalidLineContext == -1)
			;
		else
		{
			lineMgr.setFirstInvalidLineContext(Math.max(
				firstInvalidLineContext,lineIndex + 1));
		}
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

	
	
	public void indentLines(int start, int end)
	{
		try
		{
			beginCompoundEdit();
			for(int i = start; i <= end; i++)
				indentLine(i,true);
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
				indentLine(lines[i],true);
		}
		finally
		{
			endCompoundEdit();
		}
	} 

	
	
	public boolean indentLine(int lineIndex, boolean canIncreaseIndent,
		boolean canDecreaseIndent)
	{
		return indentLine(lineIndex,canDecreaseIndent);
	} 

	
	
	public boolean indentLine(int lineIndex, boolean canDecreaseIndent)
	{
		int[] whitespaceChars = new int[1];
		int currentIndent = getCurrentIndentForLine(lineIndex,
			whitespaceChars);
		int idealIndent = getIdealIndentForLine(lineIndex);

		if(idealIndent == -1 || idealIndent == currentIndent
			|| (!canDecreaseIndent && idealIndent < currentIndent))
			return false;

		
		try
		{
			beginCompoundEdit();

			int start = getLineStartOffset(lineIndex);

			remove(start,whitespaceChars[0]);
			insert(start,MiscUtilities.createWhiteSpace(
				idealIndent,(getBooleanProperty("noTabs")
				? 0 : getTabSize())));
		}
		finally
		{
			endCompoundEdit();
		}

		return true;
	} 

	
	
	public int getCurrentIndentForLine(int lineIndex, int[] whitespaceChars)
	{
		getLineText(lineIndex,seg);

		int tabSize = getTabSize();

		int currentIndent = 0;
loop:		for(int i = 0; i < seg.count; i++)
		{
			char c = seg.array[seg.offset + i];
			switch(c)
			{
			case ' ':
				currentIndent++;
				if(whitespaceChars != null)
					whitespaceChars[0]++;
				break;
			case '\t':
				currentIndent += (tabSize - (currentIndent
					% tabSize));
				if(whitespaceChars != null)
					whitespaceChars[0]++;
				break;
			default:
				break loop;
			}
		}

		return currentIndent;
	} 

	
	
	public int getIdealIndentForLine(int lineIndex)
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
						    
						    
						    
						    
						    
						    
						    
		int prevLineUnclosedParenIndex = -1; 
		int prevLineParenWeight = 0; 
		Stack openParens = new Stack();

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
				else if (c == '(')
				{
					openParens.push(new Integer(i));
					prevLineParenWeight++;
				}
				else if (c == ')')
				{
					if(openParens.size() > 0)
						openParens.pop();
					prevLineParenWeight--;
				}
						 
				break;
			}
		}

		if(openParens.size() > 0)
		{
			prevLineUnclosedParenIndex = ((Integer) openParens.pop()).intValue();
		} 

		if(Debug.INDENT_DEBUG)
		{
			Log.log(Log.DEBUG,this,"Determined previous line");
			Log.log(Log.DEBUG,this,"indent=" + indent
				+ ",prevLineBrackets=" + prevLineBrackets
				+ ",prevLineCloseBracketIndex="
				+ prevLineCloseBracketIndex);
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

		if(Debug.INDENT_DEBUG)
		{
			Log.log(Log.DEBUG,this,"Determined current line");
			Log.log(Log.DEBUG,this,"lineBrackets=" + lineBrackets
				+ ",closeBracketIndex=" + closeBracketIndex);
		}

		
		if(getBooleanProperty("deepIndent"))
		{
			if(prevLineParenWeight > 0)
			{
				indent = prevLineUnclosedParenIndex+1;
				for (int i = 0; i < prevLine.length(); i++) {
					if (prevLine.charAt(i) == '\t')
						indent += tabSize-1;
				}
				return indent;
			}
			else if(prevLineParenWeight < 0)
			{
				int openParenOffset = TextUtilities.findMatchingBracket(this,prevLineIndex,prevLine.lastIndexOf(")"));
				if(openParenOffset >= 0)
				{
					int startLine = getLineOfOffset(openParenOffset);
					int startLineParenWeight = getLineParenWeight(startLine);
					
					if(startLineParenWeight == 1)
						indent = getCurrentIndentForLine(startLine,null);
					else
						indent = getOpenParenIndent(startLine,lineIndex);
				}
			}
			
		}
		
		
		
		if(prevLineBrackets > 0)
			indent += (indentSize * prevLineBrackets);

		if(lineUpClosingBracket)
		{
			if(lineBrackets < 0)
			{
				int openBracketIndex = TextUtilities.findMatchingBracket(
					this,lineIndex,closeBracketIndex);
				if(openBracketIndex != -1)
				{
					int openLineIndex = getLineOfOffset(openBracketIndex);
					String openLine = getLineText(openLineIndex);
					Log.log(Log.DEBUG,this,"parenWeight of "+openLine+" is "+getLineParenWeight(openLineIndex));
					if (getLineParenWeight(openLineIndex) < 0)
					{
						openBracketIndex = TextUtilities.findMatchingBracket(this,openLineIndex,openLine.indexOf(")"));
						Log.log(Log.DEBUG,this,"openBracketIndex: "+openBracketIndex);
					}
					openLine = getLineText(getLineOfOffset(openBracketIndex));
					Log.log(Log.DEBUG,this,"openLine: "+openLine);
					indent = MiscUtilities.getLeadingWhiteSpaceWidth(
						openLine,tabSize);
					Log.log(Log.DEBUG,this,"indent: "+indent);
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

		
		if(lineBrackets >= 0)
		{
			
			
			if((lineBrackets == 0 || doubleBracketIndent)
				&& indentNextLinesRE != null
				&& indentNextLinesRE.isMatch(prevLine))
			{
				if(Debug.INDENT_DEBUG)
				{
					Log.log(Log.DEBUG,this,"Matches indentNextLines");
				}
				indent += indentSize;
			}
			else if(indentNextLineRE != null)
			{
				if((lineBrackets == 0 || doubleBracketIndent)
					&& indentNextLineRE.isMatch(prevLine))
					indent += indentSize;

				
				
				
				
				else if(prevLineBrackets == 0)
				{
					
					
					
					
					
					
					int prevPrevLineIndex;
					
						prevPrevLineIndex = getPriorNonEmptyLine(prevLineIndex);

					while(prevPrevLineIndex != -1)
					{
						if(indentNextLineRE.isMatch(getLineText(prevPrevLineIndex)))
							indent = getCurrentIndentForLine(prevPrevLineIndex,null);
						else
							break;

						if(Debug.INDENT_DEBUG)
						{
							Log.log(Log.DEBUG,this,
								prevPrevLineIndex
								+ " matches " +
								"indentNextLine");
						}

						prevPrevLineIndex = getPriorNonEmptyLine(prevPrevLineIndex);
					}
				}
			}
		} 

		return indent;
	} 

	
	
	private int getLineParenWeight(int line)
	{
		String lineText = getLineText(line);
		int parenWeight = 0;
		for(int i = 0; i < lineText.length(); i++)
		{
			char c = lineText.charAt(i);
			switch(c)
			{
				case '(':
					parenWeight++;
					break;
				case ')':
					parenWeight--;
					break;
				default:
			}
		}
		return parenWeight;
	} 

	
	
	private int getOpenParenIndent(int startLine, int targetLine)
	{
		Stack openParens = new Stack();
		String lineText;

		for(int lineIndex = startLine; lineIndex < targetLine; lineIndex++)
		{
			lineText = getLineText(lineIndex);
			for(int i = 0; i < lineText.length(); i++)
			{
				char c = lineText.charAt(i);
				switch(c)
				{
					case '(':
						openParens.push(new Integer(i));
						break;
					case ')':
						if(openParens.size() > 0)
							openParens.pop();
						break;
					default:
				}
			}
		}
		int indent = getCurrentIndentForLine(startLine,null);

		if(openParens.size() > 0)
			indent += ((Integer) openParens.pop()).intValue();

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

	
	
	public File getFile()
	{
		return file;
	} 

	
	
	public int getCurrentIdentForLine(int lineIndex, int[] whitespaceChars) {
		return getCurrentIndentForLine(lineIndex,whitespaceChars);
	}

	

	

	
	
	public boolean isFoldStart(int line)
	{
		return (line != getLineCount() - 1
			&& getFoldLevel(line) < getFoldLevel(line + 1));
	} 

	
	
	public boolean isFoldEnd(int line)
	{
		return (line != getLineCount() - 1
			&& getFoldLevel(line) > getFoldLevel(line + 1));
	} 

	
	
	public void invalidateCachedFoldLevels()
	{
		lineMgr.setFirstInvalidFoldLevel(0);
		fireFoldLevelChanged(0,getLineCount());
	} 

	
	
	public int getFoldLevel(int line)
	{
		if(line < 0 || line >= lineMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		if(foldHandler instanceof DummyFoldHandler)
			return 0;

		int firstInvalidFoldLevel = lineMgr.getFirstInvalidFoldLevel();
		if(firstInvalidFoldLevel == -1 || line < firstInvalidFoldLevel)
		{
			return lineMgr.getFoldLevel(line);
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
				if(newFoldLevel != lineMgr.getFoldLevel(i))
					changed = true;
				lineMgr.setFoldLevel(i,newFoldLevel);
			}

			if(line == lineMgr.getLineCount() - 1)
				lineMgr.setFirstInvalidFoldLevel(-1);
			else
				lineMgr.setFirstInvalidFoldLevel(line + 1);

			if(changed && !getFlag(TRANSACTION))
			{
				
				fireFoldLevelChanged(firstInvalidFoldLevel,line);
			}

			return newFoldLevel;
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

	
	
	public void setFoldHandler(FoldHandler foldHandler)
	{
		FoldHandler oldFoldHandler = this.foldHandler;

		if(foldHandler.equals(oldFoldHandler))
			return;

		this.foldHandler = foldHandler;

		lineMgr.setFirstInvalidFoldLevel(0);

		fireFoldHandlerChanged();
	} 

	

	

	
	
	public Position createPosition(int offset)
	{
		try
		{
			readLock();

			if(offset < 0 || offset > contentMgr.getLength())
				throw new ArrayIndexOutOfBoundsException(offset);

			return positionMgr.createPosition(offset);
		}
		finally
		{
			readUnlock();
		}
	} 

	

	

	
	
	public Vector getMarkers()
	{
		return markers;
	} 

	
	
	public String getMarkerStatusPrompt(String action)
	{
		return jEdit.getProperty("view.status." + action,
			new String[] { getMarkerNameString() });
	} 

	
	
	public String getMarkerNameString()
	{
		StringBuffer buf = new StringBuffer();
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = (Marker)markers.elementAt(i);
			if(marker.getShortcut() != '\0')
			{
				if(buf.length() != 0)
					buf.append(' ');
				buf.append(marker.getShortcut());
			}
		}

		if(buf.length() == 0)
			return jEdit.getProperty("view.status.no-markers");
		else
			return buf.toString();
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
		Marker markerN = new Marker(this,shortcut,pos);
		boolean added = false;

		
		if(!getFlag(LOADING))
		{
			if(jEdit.getBooleanProperty("persistentMarkers"))
				setDirty(true);

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
				if(jEdit.getBooleanProperty("persistentMarkers"))
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
		if(jEdit.getBooleanProperty("persistentMarkers"))
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

	
	
	public Buffer getNext()
	{
		return next;
	} 

	
	
	public Buffer getPrev()
	{
		return prev;
	} 

	
	
	public int getIndex()
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

	

	

	
	
	public LineManager _getLineManager()
	{
		return lineMgr;
	} 

	

	
	Buffer prev;
	Buffer next;

	
	Buffer(String path, boolean newFile, boolean temp, Hashtable props)
	{
		lock = new ReadWriteLock();
		propertyLock = new Object();
		contentMgr = new ContentManager();
		lineMgr = new LineManager();
		positionMgr = new PositionManager();
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

		
		if(waitSocket != null)
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
	private static final int READ_ONLY_OVERRIDE = 8;
	private static final int UNDO_IN_PROGRESS = 9;
	private static final int TEMPORARY = 10;
	private static final int TRANSACTION = 11;
	

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
	private LineManager lineMgr;
	private PositionManager positionMgr;
	private IntegerArray integerArray;
	private UndoManager undoMgr;
	private Vector bufferListeners;

	private Vector markers;

	
	private boolean textMode;
	private TokenMarker tokenMarker;
	private Segment seg;
	private FoldHandler foldHandler;

	private Socket waitSocket;
	

	
	private void setPath(String path)
	{
		this.path = path;
		VFS vfs = VFSManager.getVFSForPath(path);
		if((vfs.getCapabilities() & VFS.WRITE_CAP) == 0)
			setFlag(READ_ONLY,true);
		this.name = vfs.getFileName(path);
		this.directory = vfs.getParentOfPath(path);

		if(vfs instanceof FileVFS)
		{
			file = new File(path);
			symlinkPath = MiscUtilities.resolveSymlinks(path);

			
			
			if(autosaveFile != null)
				autosaveFile.delete();
			autosaveFile = new File(file.getParent(),'#' + name + '#');
		}
		else
		{
			
			
			file = null;
			autosaveFile = null;
			symlinkPath = path;
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

				setFlag(READ_ONLY,!file.canWrite);

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
			
			
			
			lineMgr.setFirstInvalidFoldLevel(0);

			fireFoldHandlerChanged();
		}

		
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = (Marker)markers.elementAt(i);
			marker.removePosition();
			int pos = marker.getPosition();
			if(pos > getLength())
				marker.setPosition(getLength());
			else if(pos < 0)
				marker.setPosition(0);
			marker.createPosition();
		}
	} 

	
	private void finishSaving(View view, String oldPath,
		String oldSymlinkPath, String path,
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
					jEdit.updatePosition(oldSymlinkPath,this);
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
			lineMgr.setFirstInvalidLineContext(0);
		}
	} 

	
	
	private int getPriorNonEmptyLine(int lineIndex)
	{
		int returnValue = -1;

		for(int i = lineIndex - 1; i >= 0; i--)
		{
			getLineText(i,seg);
			if(seg.count != 0)
				returnValue = i;
			for(int j = 0; j < seg.count; j++)
			{
				char ch = seg.array[seg.offset + j];
				if(!Character.isWhitespace(ch))
					return i;
			}
		}

		
		
		return returnValue;
	} 

	
	private void contentInserted(int offset, int length,
		IntegerArray endOffsets)
	{
		try
		{
			setFlag(TRANSACTION,true);

			int startLine = lineMgr.getLineOfOffset(offset);
			int numLines = endOffsets.getSize();

			lineMgr.contentInserted(startLine,offset,numLines,length,
				endOffsets);
			positionMgr.contentInserted(offset,length);

			setDirty(true);

			if(!getFlag(LOADING))
			{
				fireContentInserted(startLine,offset,numLines,length);

				if(!getFlag(UNDO_IN_PROGRESS)
					&& !insideCompoundEdit())
				{
					fireTransactionComplete();
				}
			}

		}
		finally
		{
			setFlag(TRANSACTION,false);
		}
	} 

	

	
	private BufferChangeListener getListener(int index)
	{
		return ((Listener)bufferListeners.elementAt(index)).listener;
	} 

	
	private void fireFoldLevelChanged(int start, int end)
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			try
			{
				getListener(i).foldLevelChanged(this,start,end);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+getListener(i)+" :");
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
				getListener(i).contentInserted(this,startLine,
					offset,numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+getListener(i)+" :");
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
				getListener(i).contentRemoved(this,startLine,
					offset,numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+getListener(i)+" :");
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
				getListener(i).preContentRemoved(this,startLine,
					offset,numLines,length);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+getListener(i)+" :");
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
				getListener(i).transactionComplete(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+getListener(i)+" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	private void fireFoldHandlerChanged()
	{
		for(int i = 0; i < bufferListeners.size(); i++)
		{
			try
			{
				getListener(i).foldHandlerChanged(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer event to "+getListener(i)+" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	

	
}
