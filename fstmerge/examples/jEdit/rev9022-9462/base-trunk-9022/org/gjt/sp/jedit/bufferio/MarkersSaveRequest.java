

package org.gjt.sp.jedit.bufferio;


import java.io.*;
import java.util.Vector;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.util.*;




public class MarkersSaveRequest extends WorkRequest
{
	
	public static final String ERROR_OCCURRED = "MarkersSaveRequest__error";
	

	
	
	public MarkersSaveRequest(View view, Buffer buffer,
		Object session, VFS vfs, String path)
	{
		this.view = view;
		this.buffer = buffer;
		this.session = session;
		this.vfs = vfs;
		this.path = path;
		this.markersPath = buffer.getMarkersPath(vfs);

	} 
	
	
	public void run()
	{
		OutputStream out = null;

		try
		{
			
			setAbortable(true);
			try
			{
				
				
				if((vfs.getCapabilities() & VFS.DELETE_CAP) != 0)
				{
					if(buffer.getMarkers().isEmpty())
						vfs._delete(session,markersPath,view);
					else
					{
						String[] args = { vfs.getFileName(path) };
						setStatus(jEdit.getProperty("vfs.status.save-markers",args));
						setValue(0);
						out = vfs._createOutputStream(session,markersPath,view);
						if(out != null)
							writeMarkers(out);
						}
				}
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,io);
				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
		}
		catch(WorkThread.Abort a)
		{
			if(out != null)
			{
				try
				{
					out.close();
				}
				catch(IOException io)
				{
				}
			}

			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
	} 

	
	private void writeMarkers(OutputStream out)
		throws IOException
	{
		Writer o = new BufferedWriter(new OutputStreamWriter(out));
		try
		{
			Vector markers = buffer.getMarkers();
			for(int i = 0; i < markers.size(); i++)
			{
				Marker marker = (Marker)markers.elementAt(i);
				o.write('!');
				o.write(marker.getShortcut());
				o.write(';');

				String pos = String.valueOf(marker.getPosition());
				o.write(pos);
				o.write(';');
				o.write(pos);
				o.write('\n');
			}
		}
		finally
		{
			o.close();
		}
	} 

	
	protected View view;
	protected Buffer buffer;
	protected Object session;
	protected VFS vfs;
	protected String path;
	protected String markersPath;
	

}
