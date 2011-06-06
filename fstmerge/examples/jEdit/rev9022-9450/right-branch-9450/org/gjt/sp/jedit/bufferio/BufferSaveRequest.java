

package org.gjt.sp.jedit.bufferio;


import java.io.*;
import java.util.zip.*;

import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.*;
import java.nio.charset.UnsupportedCharsetException;



public class BufferSaveRequest extends BufferIORequest
{
	
	
	public BufferSaveRequest(View view, Buffer buffer,
		Object session, VFS vfs, String path)
	{
		super(view,buffer,session,vfs,path);
	} 

	
	public void run()
	{
		

		boolean vfsRenameCap = (vfs.getCapabilities() &
			VFS.RENAME_CAP) != 0;

		boolean wantTwoStage = wantTwoStageSave(buffer);
		boolean twoStageSave = vfsRenameCap && wantTwoStage;

		try
		{
			String[] args = { vfs.getFileName(path) };
			setStatus(jEdit.getProperty("vfs.status.save",args));

			
			setAbortable(true);

			path = vfs._canonPath(session,path,view);
			if(!MiscUtilities.isURL(path))
				path = MiscUtilities.resolveSymlinks(path);

			String savePath;
			if(twoStageSave)
			{
				savePath = vfs.getTwoStageSaveName(path);
				if (savePath == null)
				{
					throw new IOException(
						"Can't get a temporary path for two-stage save: "
						+ path);
				}
			}
			else
			{
				makeBackup();
				savePath = path;
			}

			OutputStream out = vfs._createOutputStream(session,savePath,view);
			if(out == null)
			{
				buffer.setBooleanProperty(ERROR_OCCURRED,true);
				return;
			}
			try
			{
				
				
				buffer.readLock();
				try
				{
					
					
					
					if(path.endsWith(".gz"))
						buffer.setBooleanProperty(Buffer.GZIPPED,true);
					else if (buffer.getName().endsWith(".gz"))
					{
						
						
						
						
						buffer.setBooleanProperty(Buffer.GZIPPED, false);
					}

					if(buffer.getBooleanProperty(Buffer.GZIPPED))
						out = new GZIPOutputStream(out);

					write(buffer,out);
				}
				finally
				{
					buffer.readUnlock();
				}
			}
			finally
			{
				IOUtilities.closeQuietly(out);
			}

			if(twoStageSave)
			{
				makeBackup();
				if(!vfs._rename(session,savePath,path,view))
					throw new IOException("Rename failed: " + savePath);
			}

			if(!twoStageSave)
				VFSManager.sendVFSUpdate(vfs,path,true);
		}
		catch(UnsupportedCharsetException e)
		{
			Log.log(Log.ERROR, this, e, e);
			String[] pp = { e.getCharsetName() };
			VFSManager.error(view,path,"ioerror.unsupported-encoding-error",pp);

			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,this,io);
			String[] pp = { io.toString() };
			VFSManager.error(view,path,"ioerror.write-error",pp);

			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
		catch(WorkThread.Abort a)
		{
			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
		finally
		{
			try
			{
				vfs._saveComplete(session,buffer,path,view);
				if( twoStageSave )
				{
					vfs._finishTwoStageSave(session,buffer,path,view);
				}
				
				if(!jEdit.getBooleanProperty("persistentMarkers"))
					vfs._delete(session,Buffer.getMarkersPath(vfs, path),view);
				vfs._endVFSSession(session,view);
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,io);
				String[] pp = { io.toString() };
				VFSManager.error(view,path,"ioerror.write-error",pp);

				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
			catch(WorkThread.Abort a)
			{
				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
		}
	} 

	
	
	private void makeBackup() throws IOException
	{
		
		if(buffer.getProperty(Buffer.BACKED_UP) == null
			|| jEdit.getBooleanProperty("backupEverySave"))
		{
			vfs._backup(session,path,view);
			buffer.setBooleanProperty(Buffer.BACKED_UP,true);
		}
	} 

	
	public static boolean wantTwoStageSave(Buffer buffer)
	{
		return !buffer.getBooleanProperty("forbidTwoStageSave") &&
			(buffer.getBooleanProperty("overwriteReadonly") ||
			jEdit.getBooleanProperty("twoStageSave"));
	}
}
