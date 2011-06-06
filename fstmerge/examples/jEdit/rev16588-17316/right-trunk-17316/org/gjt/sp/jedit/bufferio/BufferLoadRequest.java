

package org.gjt.sp.jedit.bufferio;


import java.io.*;
import java.nio.charset.*;
import java.util.List;
import java.util.ArrayList;
import java.util.Set;
import java.util.HashSet;
import java.util.zip.GZIPInputStream;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.util.*;



public class BufferLoadRequest extends BufferIORequest
{
	
	
	public BufferLoadRequest(View view, Buffer buffer,
		Object session, VFS vfs, String path)
	{
		super(view,buffer,session,vfs,path);
	} 
	
	
	public void run()
	{
		try
		{
			setAbortable(true);
			if(!buffer.isTemporary())
			{
				String[] args = { vfs.getFileName(path) };
				setStatus(jEdit.getProperty("vfs.status.load",args));
				setValue(0L);
			}

			path = vfs._canonPath(session,path,view);

			readContents();
			buffer.setNewFile(false);

			if (jEdit.getBooleanProperty("persistentMarkers") &&
			    (vfs.isMarkersFileSupported()))
			{
				InputStream markers = null;
				try
				{
					String[] args = { vfs.getFileName(path) };
					if(!buffer.isTemporary())
						setStatus(jEdit.getProperty("vfs.status.load-markers",args));
					setAbortable(true);

					markers = vfs._createInputStream(session,markersPath,true,view);
					if(markers != null)
						readMarkers(buffer,markers);
				}
				catch(Exception e)
				{
					
				}
				finally
				{
					IOUtilities.closeQuietly(markers);
				}
			}
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,e);
			Object[] pp = { e.toString() };
			VFSManager.error(view,path,"ioerror.read-error",pp);

			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
		catch(OutOfMemoryError oom)
		{
			Log.log(Log.ERROR,this,oom);
			VFSManager.error(view,path,"out-of-memory-error",null);

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
				vfs._endVFSSession(session,view);
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,this,e);
				String[] pp = { e.toString() };
				VFSManager.error(view,path,"ioerror.read-error",pp);

				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
			catch(WorkThread.Abort a)
			{
				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
		}
	} 

	
	
	private InputStream getNakedStream() throws IOException
	{
		InputStream in = vfs._createInputStream(session,path,false,view);
		if(in != null)
		{
			return in;
		}
		throw new IOException("Unable to get a Stream for " + path);
	} 

	
	
	private long getContentLength() throws IOException
	{
		VFSFile entry = vfs._getFile(session,path,view);
		if(entry != null)
			return entry.getLength();
		else
			return 0L;
	} 

	
	
	private BufferedInputStream rewindContentsStream(BufferedInputStream markedStream, boolean gzipped)
		throws IOException
	{
		try
		{
			markedStream.reset();
			return markedStream;
		}
		catch(IOException e)
		{
			Log.log(Log.NOTICE, this
				, path + ": Reopening to rewind the stream");
			
			
			markedStream.close();
			InputStream in = getNakedStream();
			try
			{
				if(gzipped)
				{
					in = new GZIPInputStream(in);
				}
				BufferedInputStream result
					= AutoDetection.getMarkedStream(in);
				in = null;
				return result;
			}
			finally
			{
				IOUtilities.closeQuietly(in);
			}
		}
	} 

	
	
	private void readContents() throws IOException
	{
		long length = getContentLength();

		BufferedInputStream markedStream
			= AutoDetection.getMarkedStream(getNakedStream());
		try
		{
			boolean gzipped = false;
			
			
			
			List<Object> encodingProviders
				= new ArrayList<Object>();

			boolean autodetect = buffer.getBooleanProperty(Buffer.ENCODING_AUTODETECT);
			if(autodetect)
			{
				gzipped = AutoDetection.isGzipped(markedStream);
				markedStream.reset();

				encodingProviders.addAll(AutoDetection.getEncodingDetectors());
				
				
				encodingProviders.add(buffer.getStringProperty(Buffer.ENCODING));

				String fallbackEncodings = jEdit.getProperty("fallbackEncodings");
				if(fallbackEncodings != null && fallbackEncodings.length() > 0)
				{
					for(String encoding: fallbackEncodings.split("\\s+"))
					{
						encodingProviders.add(encoding);
					}
				}
			}
			else
			{
				gzipped = buffer.getBooleanProperty(Buffer.GZIPPED);
				encodingProviders.add(buffer.getStringProperty(Buffer.ENCODING));
			}

			if(gzipped)
			{
				Log.log(Log.DEBUG, this, path + ": Stream is gzipped.");
				markedStream = AutoDetection.getMarkedStream(
					new GZIPInputStream(markedStream));
			}

			Set<String> failedEncodings = new HashSet<String>();
			Exception encodingError = null;
			for(Object encodingProvider: encodingProviders)
			{
				String encoding = null;
				if (encodingProvider instanceof String)
				{
					encoding = (String)encodingProvider;
				}
				else if(encodingProvider instanceof EncodingDetector)
				{
					markedStream = rewindContentsStream(markedStream, gzipped);
					encoding = ((EncodingDetector)encodingProvider).detectEncoding(new BufferedInputStream(markedStream));
				}
				else
				{
					Log.log(Log.DEBUG, this, "Strange encodingProvider: " + encodingProvider);
				}

				if(encoding == null || encoding.length() <= 0
					|| failedEncodings.contains(encoding))
				{
					continue;
				}

				markedStream = rewindContentsStream(markedStream, gzipped);
				try
				{
					read(EncodingServer.getTextReader(markedStream, encoding)
						, length, false);
					if(autodetect)
					{
						
						if(gzipped)
						{
							buffer.setBooleanProperty(Buffer.GZIPPED,true);
						}
						buffer.setProperty(Buffer.ENCODING, encoding);
					}
					return;
				}
				catch(CharConversionException e)
				{
					encodingError = e;
				}
				catch(CharacterCodingException e)
				{
					encodingError = e;
				}
				catch(UnsupportedEncodingException e)
				{
					encodingError = e;
				}
				catch(UnsupportedCharsetException e)
				{
					encodingError = e;
				}
				Log.log(Log.NOTICE, this, path + ": " + encoding
					+ ": " + encodingError);
				failedEncodings.add(encoding);
			}
			
			Object[] pp = { TextUtilities.join(failedEncodings,","), "" };
			if(failedEncodings.size() < 2)
			{
				pp[1] = encodingError.toString();
			}
			else
			{
				pp[1] = "See details in Activity Log";
			}
			VFSManager.error(view,path,"ioerror.encoding-error",pp);
			markedStream = rewindContentsStream(markedStream, gzipped);
			read(EncodingServer.getEncoding(
				buffer.getStringProperty(Buffer.ENCODING)
				).getPermissiveTextReader(markedStream)
				, length, false);
			if(autodetect && gzipped)
			{
				buffer.setBooleanProperty(Buffer.GZIPPED,true);
			}
		}
		finally
		{
			markedStream.close();
		}
	} 

	
	private static void readMarkers(Buffer buffer, InputStream _in)
		throws IOException
	{
		
		buffer.removeAllMarkers();

		BufferedReader in = new BufferedReader(new InputStreamReader(_in));

		try
		{
			String line;
			while((line = in.readLine()) != null)
			{
				
				if(line.length() == 0)
					continue;
				
				
				if(line.charAt(0) != '!')
					continue;


				char shortcut = line.charAt(1);
				int start = line.indexOf(';');
				int end = line.indexOf(';',start + 1);
				int position = Integer.parseInt(line.substring(start + 1,end));
				buffer.addMarker(shortcut,position);
			}
			buffer.setMarkersChanged(false);
		}
		finally
		{
			in.close();
		}
	} 
}
