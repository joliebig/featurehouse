

package org.gjt.sp.jedit;


import org.gjt.sp.jedit.browser.VFSBrowser;
import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.bufferio.BufferAutosaveRequest;
import org.gjt.sp.jedit.bufferio.BufferIORequest;
import org.gjt.sp.jedit.bufferio.MarkersSaveRequest;
import org.gjt.sp.jedit.gui.StyleEditor;
import org.gjt.sp.jedit.io.FileVFS;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.io.VFSFile;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.msg.BufferUpdate;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.util.IntegerArray;
import org.gjt.sp.util.Log;
import org.gjt.sp.jedit.visitors.JEditVisitorAdapter;
import org.gjt.sp.jedit.visitors.SaveCaretInfoVisitor;
import org.gjt.sp.jedit.options.GeneralOptionPane;

import javax.swing.*;
import javax.swing.text.AttributeSet;
import javax.swing.text.Segment;
import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.*;



public class Buffer extends JEditBuffer
{
	
	
	public static final String BACKED_UP = "Buffer__backedUp";

	
	public static final String CARET = "Buffer__caret";
	public static final String CARET_POSITIONED = "Buffer__caretPositioned";

	
	public static final String SELECTION = "Buffer__selection";

	
	public static final String SCROLL_VERT = "Buffer__scrollVert";
	public static final String SCROLL_HORIZ = "Buffer__scrollHoriz";

	
	public static final String ENCODING_AUTODETECT = "encodingAutodetect";

	
	public static final String TRAILING_EOL = "trailingEOL";

	
	public static final String GZIPPED = "gzipped";
	

	

	
	
	public void reload(View view)
	{
		if (getFlag(UNTITLED))
			return;
		if(isDirty())
		{
			String[] args = { path };
			int result = GUIUtilities.confirm(view,"changedreload",
				args,JOptionPane.YES_NO_OPTION,
				JOptionPane.WARNING_MESSAGE);
			if(result != JOptionPane.YES_OPTION)
				return;
		}
		view.visit(new SaveCaretInfoVisitor());
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

		setLoading(true);

		
		
		
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
					setLoading(false);
					return false;
				}

				
				
				if(reload || !getFlag(NEW_FILE))
				{
					if(!vfs.load(view,this,path))
					{
						setLoading(false);
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

				loadText(seg,endOffsets);

				unsetProperty(BufferIORequest.LOAD_DATA);
				unsetProperty(BufferIORequest.END_OFFSETS);
				unsetProperty(BufferIORequest.NEW_PATH);

				undoMgr.clear();
				undoMgr.setLimit(jEdit.getIntegerProperty(
					"buffer.undoCount",100));

				if(!getFlag(TEMPORARY))
					finishLoading();

				setLoading(false);

				
				if(reload)
					setDirty(false);

				if(!loadAutosave && newPath != null)
					setPath(newPath);

				
				

				
				
				
				
				
				
				if(loadAutosave)
					Buffer.super.setDirty(true);

				
				if(!getFlag(TEMPORARY))
				{
					fireBufferLoaded();
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

	
	
	public boolean insertFile(View view, String path)
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
			|| !isDirty() || isPerformingIO() ||
			!autosaveFile.getParentFile().exists())
			return;

		setFlag(AUTOSAVE_DIRTY,false);

		VFSManager.runInWorkThread(new BufferAutosaveRequest(
			null,this,null,VFSManager.getFileVFS(),
			autosaveFile.getPath()));
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
		return save(view,path,true,false);
	} 

	
	
	public boolean save(View view, String path, boolean rename)
	{
		return save(view,path,rename,false);
	} 

	
	
	public boolean save(final View view, String path, final boolean rename, boolean disableFileStatusCheck)
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

		setPerformingIO(true);

		final String oldPath = this.path;
		final String oldSymlinkPath = symlinkPath;
		final String newPath = path == null ? this.path : path;

		VFS vfs = VFSManager.getVFSForPath(newPath);

		if(!checkFileForSave(view,vfs,newPath))
		{
			setPerformingIO(false);
			return false;
		}

		Object session = vfs.createVFSSession(newPath,view);
		if (session == null)
		{
			setPerformingIO(false);
			return false;
		}

		unsetProperty("overwriteReadonly");
		unsetProperty("forbidTwoStageSave");
		try
		{
			VFSFile file = vfs._getFile(session,newPath,view);
			if (file != null)
			{
				boolean vfsRenameCap = (vfs.getCapabilities() & VFS.RENAME_CAP) != 0;
				if (!file.isWriteable())
				{
					Log.log(Log.WARNING, this, "Buffer saving : File " + file + " is readOnly");
					if (vfsRenameCap)
					{
						Log.log(Log.DEBUG, this, "Buffer saving : VFS can rename files");
						String savePath = vfs._canonPath(session,newPath,view);
						if(!MiscUtilities.isURL(savePath))
							savePath = MiscUtilities.resolveSymlinks(savePath);
						savePath = vfs.getTwoStageSaveName(savePath);
						if (savePath == null)
						{
							Log.log(Log.DEBUG, this, "Buffer saving : two stage save impossible because path is null");
							VFSManager.error(view,
								newPath,
								"ioerror.save-readonly-twostagefail",
								null);
							setPerformingIO(false);
							return false;
						}
						else
						{
							int result = GUIUtilities.confirm(
								view, "vfs.overwrite-readonly",
								new Object[]{newPath},
								JOptionPane.YES_NO_OPTION,
								JOptionPane.WARNING_MESSAGE);
							if (result == JOptionPane.YES_OPTION)
							{
								Log.log(Log.WARNING, this, "Buffer saving : two stage save will be used to save buffer");
								setBooleanProperty("overwriteReadonly",true);
							}
							else
							{
								Log.log(Log.DEBUG,this, "Buffer not saved");
								setPerformingIO(false);
								return false;
							}
						}
					}
					else
					{
						Log.log(Log.WARNING, this, "Buffer saving : file is readonly and vfs cannot do two stage save");
						VFSManager.error(view,
							newPath,
							"ioerror.write-error-readonly",
							null);
						setPerformingIO(false);
						return false;
					}
				}
				else
				{
					String savePath = vfs._canonPath(session,newPath,view);
					if(!MiscUtilities.isURL(savePath))
						savePath = MiscUtilities.resolveSymlinks(savePath);
					savePath = vfs.getTwoStageSaveName(savePath);
					if (jEdit.getBooleanProperty("twoStageSave") && (!vfsRenameCap || savePath == null))
					{
						
						


						int result = GUIUtilities.confirm(
								view, "vfs.twostageimpossible",
								new Object[]{newPath},
								JOptionPane.YES_NO_OPTION,
								JOptionPane.WARNING_MESSAGE);
						if (result == JOptionPane.YES_OPTION)
						{
							Log.log(Log.WARNING, this, "Buffer saving : two stage save cannot be used");
							setBooleanProperty("forbidTwoStageSave",true);
						}
						else
						{
							Log.log(Log.DEBUG,this, "Buffer not saved");
							setPerformingIO(false);
							return false;
						}

					}
				}
			}
		}
		catch(IOException io)
		{
			VFSManager.error(view,newPath,"ioerror",
				new String[] { io.toString() });
			setPerformingIO(false);
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
				VFSManager.error(view,newPath,"ioerror",
					new String[] { io.toString() });
				setPerformingIO(false);
				return false;
			}
		}

		if(!vfs.save(view,this,newPath))
		{
			setPerformingIO(false);
			return false;
		}

		
		VFSManager.runInAWTThread(new Runnable()
			{
				public void run()
				{
					setPerformingIO(false);
					setProperty("overwriteReadonly",null);
					finishSaving(view,oldPath,oldSymlinkPath,
						newPath,rename,getBooleanProperty(
							BufferIORequest.ERROR_OCCURRED));
					updateMarkersFile(view);
				}
			});

		int check = jEdit.getIntegerProperty("checkFileStatus");
		if(!disableFileStatusCheck && (check == GeneralOptionPane.checkFileStatus_all ||
					       check == GeneralOptionPane.checkFileStatus_operations))
			jEdit.checkBufferStatus(view,false);

		return true;
	} 

	
	public static final int FILE_NOT_CHANGED = 0;
	public static final int FILE_CHANGED = 1;
	public static final int FILE_DELETED = 2;
	
	public int checkFileStatus(View view)
	{
		
		
		
		
		if(!isPerformingIO() && file != null && !getFlag(NEW_FILE))
		{
			boolean newReadOnly = file.exists() && !file.canWrite();
			if(newReadOnly != isFileReadOnly())
			{
				setFileReadOnly(newReadOnly);
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

	
	
	public boolean getAutoReload()
	{
		return getFlag(AUTORELOAD);
	} 

	
	
	public void setAutoReload(boolean value)
	{
		setFlag(AUTORELOAD, value);
	} 

	
	
	public boolean getAutoReloadDialog()
	{
		return getFlag(AUTORELOAD_DIALOG);
	} 

	
	
	public void setAutoReloadDialog(boolean value)
	{
		setFlag(AUTORELOAD_DIALOG, value);
	} 

	
	
	public VFS getVFS()
	{
		return VFSManager.getVFSForPath(path);
	} 

	
	
	public File getAutosaveFile()
	{
		return autosaveFile;
	} 

	
	
	public void removeAutosaveFile()
	{
		if (autosaveFile != null)
		{
			autosaveFile.delete();
			setFlag(AUTOSAVE_DIRTY,true);
		}
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public String getPath()
	{
		return path;
	} 

	
	
	public String getPath(Boolean shortVersion)
	{
		return shortVersion ? MiscUtilities.abbreviate(path) : getPath();
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
		return !isLoading();
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

	
	
	@Override
	public void setDirty(boolean d)
	{
		boolean old_d = isDirty();
		if (isUntitled() && jEdit.getBooleanProperty("suppressNotSavedConfirmUntitled"))
			d = false;
		if (d && getLength() == initialLength)
		{
			if (jEdit.getBooleanProperty("useMD5forDirtyCalculation")) 
				d = !Arrays.equals(calculateHash(), md5hash);
		}
		super.setDirty(d);
		boolean editable = isEditable();

		if(d)
		{
			if(editable)
				setFlag(AUTOSAVE_DIRTY,true);
		}
		else
		{
			setFlag(AUTOSAVE_DIRTY,false);

			if(autosaveFile != null)
				autosaveFile.delete();
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
		if(isDirty())
			return GUIUtilities.loadIcon("dirty.gif");
		else if(isReadOnly())
			return GUIUtilities.loadIcon("readonly.gif");
		else if(getFlag(NEW_FILE))
			return GUIUtilities.loadIcon("new.gif");
		else
			return GUIUtilities.loadIcon("normal.gif");
	} 

	

	

	
	
	@Deprecated
	public void addBufferChangeListener(BufferChangeListener listener,
		int priority)
	{
		addBufferListener(new BufferChangeListener.Adapter(listener),priority);
	} 

	
	
	@Deprecated
	public void addBufferChangeListener(BufferChangeListener listener)
	{
		addBufferListener(new BufferChangeListener.Adapter(listener), NORMAL_PRIORITY);
	} 

	
	
	@Deprecated
	public void removeBufferChangeListener(BufferChangeListener listener)
	{
		BufferListener[] listeners = getBufferListeners();

		for(int i = 0; i < listeners.length; i++)
		{
			BufferListener l = listeners[i];
			if(l instanceof BufferChangeListener.Adapter)
			{
				if(((BufferChangeListener.Adapter)l).getDelegate() == listener)
				{
					removeBufferListener(l);
					return;
				}
			}
		}
	} 

	

	

	
	
	@Override
	public void propertiesChanged()
	{
		super.propertiesChanged();
		setAutoReloadDialog(jEdit.getBooleanProperty("autoReloadDialog"));
		setAutoReload(jEdit.getBooleanProperty("autoReload"));
		EditBus.send(new BufferUpdate(this,null,BufferUpdate.PROPERTIES_CHANGED));
	} 

	
	@Override
	public Object getDefaultProperty(String name)
	{
		Object retVal;

		if(mode != null)
		{
			retVal = mode.getProperty(name);
			if(retVal == null)
				return null;

			setDefaultProperty(name,retVal);
			return retVal;
		}
		
		String value = jEdit.getProperty("buffer." + name);
		if(value == null)
			return null;

		
		try
		{
			retVal = new Integer(value);
		}
		catch(NumberFormatException nf)
		{
			retVal = value;
		}

		return retVal;
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
		String lineSep = getStringProperty(LINESEP);
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
		setProperty(LINESEP, lineSep);
		setDirty(true);
		propertiesChanged();
	} 

	
	
	@Override
	public String getContextSensitiveProperty(int offset, String name)
	{
		Object value = super.getContextSensitiveProperty(offset,name);

		if(value == null)
		{
			ParserRuleSet rules = getRuleSetAtOffset(offset);

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

	

	

	
	
	public void setMode()
	{
		String userMode = getStringProperty("mode");
		if(userMode != null)
		{
			unsetProperty("mode");
			Mode m = ModeProvider.instance.getMode(userMode);
			if(m != null)
			{
				setMode(m);
				return;
			}
		}

		String firstLine = getLineText(0);

		Mode mode = ModeProvider.instance.getModeForFile(name, firstLine);
		if (mode != null)
		{
			setMode(mode);
			return;
		}

		Mode defaultMode = jEdit.getMode(jEdit.getProperty("buffer.defaultMode"));
		if(defaultMode == null)
			defaultMode = jEdit.getMode("text");

		if (defaultMode != null)
			setMode(defaultMode);
	} 

	

	

	
	
	@Deprecated
	public void putProperty(Object name, Object value)
	{
		
		if(!(name instanceof String))
			return;

		setProperty((String)name,value);
	} 

	
	
	@Deprecated
	public void putBooleanProperty(String name, boolean value)
	{
		setBooleanProperty(name,value);
	} 

	
	
	@Deprecated
	public static class TokenList extends DefaultTokenHandler
	{
		public Token getFirstToken()
		{
			return getTokens();
		}
	}

	
	@Deprecated
	public TokenList markTokens(int lineIndex)
	{
		TokenList list = new TokenList();
		markTokens(lineIndex,list);
		return list;
	} 

	
	
	@Deprecated
	public void insertString(int offset, String str, AttributeSet attr)
	{
		insert(offset,str);
	} 

	
	
	@Deprecated
	public File getFile()
	{
		return file;
	} 

	

	

	
	
	public Vector<Marker> getMarkers()
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
		StringBuilder buf = new StringBuilder();
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = markers.get(i);
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

		
		if(isLoaded())
		{
			setFlag(MARKERS_CHANGED,true);

			markerN.createPosition();

			for(int i = 0; i < markers.size(); i++)
			{
				Marker marker = markers.get(i);
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
				Marker marker = markers.get(i);
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

		if(isLoaded() && !getFlag(TEMPORARY))
		{
			EditBus.send(new BufferUpdate(this,null,
				BufferUpdate.MARKERS_CHANGED));
		}
	} 

	
	
	public Marker getMarkerInRange(int start, int end)
	{
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = markers.get(i);
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
			Marker marker = markers.get(i);
			if(getLineOfOffset(marker.getPosition()) == line)
				return marker;
		}

		return null;
	} 

	
	
	public void removeMarker(int line)
	{
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = markers.get(i);
			if(getLineOfOffset(marker.getPosition()) == line)
			{
				setFlag(MARKERS_CHANGED,true);
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
		setFlag(MARKERS_CHANGED,true);

		for(int i = 0; i < markers.size(); i++)
			markers.get(i).removePosition();

		markers.removeAllElements();

		if(isLoaded())
		{
			EditBus.send(new BufferUpdate(this,null,
				BufferUpdate.MARKERS_CHANGED));
		}
	} 

	
	
	public Marker getMarker(char shortcut)
	{
		for (Marker marker : markers)
		{
			if(marker.getShortcut() == shortcut)
				return marker;
		}
		return null;
	} 

	
	
	@Deprecated
	public String getMarkersPath(VFS vfs)
	{
		return getMarkersPath(vfs, path);
	} 

	
	
	public static String getMarkersPath(VFS vfs, String path)
	{
		return vfs.getParentOfPath(path)
			+ '.' + vfs.getFileName(path)
			+ ".marks";
	} 

	
	
	public boolean updateMarkersFile(View view)
	{
		if(!markersChanged())
			return true;
		
		VFS vfs = VFSManager.getVFSForPath(getPath());
		if (((vfs.getCapabilities() & VFS.WRITE_CAP) == 0) ||
		    !vfs.isMarkersFileSupported())
		{
			VFSManager.error(view, path, "vfs.not-supported.save",
				new String[] { "markers file" });
			return false;
		}
		Object session = vfs.createVFSSession(path, view);
		if(session == null)
			return false;
		VFSManager.runInWorkThread(
			new MarkersSaveRequest(
				view, this, session, vfs, path));
		return true;
	} 

	
	
	public boolean markersChanged()
	{
		return getFlag(MARKERS_CHANGED);
	} 

	
	
	public void setMarkersChanged(boolean changed)
	{
		setFlag(MARKERS_CHANGED, changed);
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
		while (true)
		{
			if(buffer == null)
				break;
			count++;
			buffer = buffer.prev;
		}
		return count;
	} 

	
	
	@Override
	public String toString()
	{
		return name + " (" + MiscUtilities.abbreviate(directory) + ')';
	} 

	
	
	public void addBufferUndoListener(BufferUndoListener listener)
	{
		undoListeners.add(listener);
	} 

	
	
	public void removeBufferUndoListener(BufferUndoListener listener)
	{
		undoListeners.remove(listener);
	} 

	

	
	
	Buffer prev;
	
	Buffer next;

	
	Buffer(String path, boolean newFile, boolean temp, Map props)
	{
		super(props);

		markers = new Vector<Marker>();

		setFlag(TEMPORARY,temp);

		
		setPath(path);

		
		setFlag(UNTITLED,newFile);
		setFlag(NEW_FILE,newFile);
		setFlag(AUTORELOAD,jEdit.getBooleanProperty("autoReload"));
		setFlag(AUTORELOAD_DIALOG,jEdit.getBooleanProperty("autoReloadDialog"));

		undoListeners = new Vector<BufferUndoListener>();
	} 

	
	void commitTemporary()
	{
		setFlag(TEMPORARY,false);

		finishLoading();
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

	

	

	
	protected void fireBeginUndo()
	{
		for (BufferUndoListener listener: undoListeners)
		{
			try
			{
				listener.beginUndo(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer undo event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireEndUndo()
	{
		for (BufferUndoListener listener: undoListeners)
		{
			try
			{
				listener.endUndo(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer undo event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireBeginRedo()
	{
		for (BufferUndoListener listener: undoListeners)
		{
			try
			{
				listener.beginRedo(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer begin redo event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 

	
	protected void fireEndRedo()
	{
		for (BufferUndoListener listener: undoListeners)
		{
			try
			{
				listener.endRedo(this);
			}
			catch(Throwable t)
			{
				Log.log(Log.ERROR,this,"Exception while sending buffer end redo event to "+ listener +" :");
				Log.log(Log.ERROR,this,t);
			}
		}
	} 
	

	

	

	
	private void setFlag(int flag, boolean value)
	{
		if(value)
			flags |= 1 << flag;
		else
			flags &= ~(1 << flag);
	} 

	
	private boolean getFlag(int flag)
	{
		int mask = 1 << flag;
		return (flags & mask) == mask;
	} 

	
	private static final int CLOSED = 0;
	private static final int NEW_FILE = 3;
	private static final int UNTITLED = 4;
	private static final int AUTOSAVE_DIRTY = 5;
	private static final int AUTORELOAD = 6;
	private static final int AUTORELOAD_DIALOG = 7;
	private static final int TEMPORARY = 10;
	private static final int MARKERS_CHANGED = 12;
	

	private int flags;

	

	
	private String path;
	private String symlinkPath;
	private String name;
	private String directory;
	private File file;
	private File autosaveFile;
	private long modTime;
	private byte[] md5hash;
	private int initialLength;

	private final Vector<Marker> markers;

	private Socket waitSocket;
	private List<BufferUndoListener> undoListeners;
	

	
	private void setPath(final String path)
	{
		jEdit.visit(new JEditVisitorAdapter()
		{
			@Override
			public void visit(EditPane editPane)
			{
				editPane.bufferRenamed(Buffer.this.path, path);
			}
		});

		this.path = path;
		VFS vfs = VFSManager.getVFSForPath(path);
		if((vfs.getCapabilities() & VFS.WRITE_CAP) == 0)
			setFileReadOnly(true);
		name = vfs.getFileName(path);
		directory = vfs.getParentOfPath(path);

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
			Object session = vfs.createVFSSession(path, view);
			if(session == null)
				return false;

			try
			{
				VFSFile file = vfs._getFile(session,path,view);
				if(file == null)
				{
					setNewFile(true);
					return true;
				}

				if(!file.isReadable())
				{
					VFSManager.error(view,path,"ioerror.no-read",null);
					setNewFile(false);
					return false;
				}

				setFileReadOnly(!file.isWriteable());

				if(file.getType() != VFSFile.FILE)
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

	
	private static boolean checkFileForSave(View view, VFS vfs, String path)
	{
		if((vfs.getCapabilities() & VFS.LOW_LATENCY_CAP) != 0)
		{
			Object session = vfs.createVFSSession(path,view);
			if(session == null)
				return false;

			try
			{
				VFSFile file = vfs._getFile(session,path,view);
				if(file == null)
					return true;

				if(file.getType() != VFSFile.FILE)
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

	
	private byte[] calculateHash()
	{
		final byte[] dummy = new byte[1]; 
		if (!jEdit.getBooleanProperty("useMD5forDirtyCalculation"))
			return dummy;
		ByteBuffer bb = null;
		readLock();
		try 
		{
			
			int length = getLength();
			bb = ByteBuffer.allocate(length * 2);	
			CharBuffer cb = bb.asCharBuffer();
			cb.append( getSegment(0, length) );
		}
		finally 
		{
			readUnlock();
		} 
		try 
		{
			MessageDigest digest = java.security.MessageDigest.getInstance("MD5");
			digest.update( bb );
			return digest.digest();
		}
		catch (NoSuchAlgorithmException nsae) 
		{
			Log.log(Log.ERROR, this, "Can't Calculate MD5 hash!", nsae);
			return dummy;
		}
		
	}
	
	
	private void updateHash() 
	{
		initialLength = getLength();
		md5hash = calculateHash();
	}
	
	
	private void finishLoading()
	{
		updateHash();
			
		parseBufferLocalProperties();
		
		
		FoldHandler oldFoldHandler = getFoldHandler();
		setMode();

		if(getFoldHandler() == oldFoldHandler)
		{
			
			
			
			invalidateFoldLevels();

			fireFoldHandlerChanged();
		}

		
		for(int i = 0; i < markers.size(); i++)
		{
			Marker marker = markers.get(i);
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
					setFileReadOnly(false);
					setFlag(NEW_FILE,false);
					setFlag(UNTITLED,false);
					super.setDirty(false);
					if(jEdit.getBooleanProperty("resetUndoOnSave"))
					{
						undoMgr.clear();
					}
				}
				finally
				{
					writeUnlock();
				}

				parseBufferLocalProperties();

				if(!getPath().equals(oldPath))
				{
					if (!isTemporary())
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

				updateHash();
				
				if (!isTemporary())
				{
					EditBus.send(new BufferUpdate(this,
								      view,BufferUpdate.DIRTY_CHANGED));

					
					EditBus.send(new BufferUpdate(this,
								      view,BufferUpdate.SAVED));
				}
			}
		} 
	} 

	
	
	public void editSyntaxStyle(JEditTextArea textArea)
	{
		int lineNum = textArea.getCaretLine();
		int start = getLineStartOffset(lineNum);
		int position = textArea.getCaretPosition();

		DefaultTokenHandler tokenHandler = new DefaultTokenHandler();
		markTokens(lineNum,tokenHandler);
		Token token = tokenHandler.getTokens();

		while(token.id != Token.END)
		{
			int next = start + token.length;
			if (start <= position && next > position)
				break;
			start = next;
			token = token.next;
		}
		if (token.id == Token.END || token.id == Token.NULL)
		{
			JOptionPane.showMessageDialog(jEdit.getActiveView(),
				jEdit.getProperty("syntax-style-no-token.message"),
				jEdit.getProperty("syntax-style-no-token.title"),
				JOptionPane.PLAIN_MESSAGE);
			return;
		}
		String typeName = Token.tokenToString(token.id);
		String property = "view.style." + typeName.toLowerCase();
		SyntaxStyle currentStyle = GUIUtilities.parseStyle(
				jEdit.getProperty(property), "Dialog",12);
		SyntaxStyle style = new StyleEditor(jEdit.getActiveView(),
				currentStyle, typeName).getStyle();
		if(style != null)
		{
			jEdit.setProperty(property, GUIUtilities.getStyleString(style));
			jEdit.propertiesChanged();
		}
	} 
	
}
