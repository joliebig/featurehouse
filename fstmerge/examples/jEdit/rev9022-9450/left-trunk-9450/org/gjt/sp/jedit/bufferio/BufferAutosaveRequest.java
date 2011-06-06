

package org.gjt.sp.jedit.bufferio;


import java.io.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.*;



public class BufferAutosaveRequest extends BufferIORequest
{
	
	
	public BufferAutosaveRequest(View view, Buffer buffer,
		Object session, VFS vfs, String path)
	{
		super(view,buffer,session,vfs,path);
	} 

	
	public void run()
	{
		OutputStream out = null;

		try
		{
			String[] args = { vfs.getFileName(path) };
			setStatus(jEdit.getProperty("vfs.status.autosave",args));

			
			setAbortable(true);

			try
			{
				

				if(!buffer.isDirty())
				{
					
					
					return;
				}

				out = vfs._createOutputStream(session,path,view);
				if(out == null)
					return;

				write(buffer,out);
			}
			catch(Exception e)
			{
			}
			
			
				
			
		}
		catch(WorkThread.Abort a)
		{
		}
		finally
		{
			IOUtilities.closeQuietly(out);
		}
	} 
}
