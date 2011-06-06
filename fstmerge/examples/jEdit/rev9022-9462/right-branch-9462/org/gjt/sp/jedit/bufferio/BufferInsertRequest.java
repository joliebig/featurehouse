

package org.gjt.sp.jedit.bufferio;


import java.io.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.*;



public class BufferInsertRequest extends BufferIORequest
{
	
	
	public BufferInsertRequest(View view, Buffer buffer,
		Object session, VFS vfs, String path)
	{
		super(view,buffer,session,vfs,path);
	} 

	
	public void run()
	{
		InputStream in = null;
		try
		{
			String[] args = { vfs.getFileName(path) };
			setStatus(jEdit.getProperty("vfs.status.load",args));
			setAbortable(true);

			path = vfs._canonPath(session,path,view);

			VFSFile entry = vfs._getFile(
				session,path,view);
			long length;
			if(entry != null)
				length = entry.getLength();
			else
				length = 0L;

			in = vfs._createInputStream(session,path,false,view);
			if(in == null)
				return;

			final SegmentBuffer seg = read(
				autodetect(in),length,true);

			
			VFSManager.runInAWTThread(new Runnable()
			{
				public void run()
				{
					view.getTextArea().setSelectedText(
						seg.toString());
				}
			});
		}
		catch(IOException io)
		{
			Log.log(Log.ERROR,this,io);
			String[] pp = { io.toString() };
			VFSManager.error(view,path,"ioerror.read-error",pp);

			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
		catch(WorkThread.Abort a)
		{
			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
		finally
		{
			IOUtilities.closeQuietly(in);
			try
			{
				vfs._endVFSSession(session,view);
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,io);
				String[] pp = { io.toString() };
				VFSManager.error(view,path,"ioerror.read-error",pp);

				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
			catch(WorkThread.Abort a)
			{
				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
		}
	} 
}
