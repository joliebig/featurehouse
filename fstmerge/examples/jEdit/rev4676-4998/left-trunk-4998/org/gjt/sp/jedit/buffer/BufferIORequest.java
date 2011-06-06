

package org.gjt.sp.jedit.buffer;


import javax.swing.text.Segment;
import java.io.*;
import java.util.zip.*;
import java.util.Vector;
import org.gjt.sp.jedit.io.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.*;



public class BufferIORequest extends WorkRequest
{
	
	
	public static final int IOBUFSIZE = 32768;

	
	public static final int PROGRESS_INTERVAL = 300;

	public static final String LOAD_DATA = "BufferIORequest__loadData";
	public static final String END_OFFSETS = "BufferIORequest__endOffsets";
	public static final String NEW_PATH = "BufferIORequest__newPath";

	
	public static final String ERROR_OCCURRED = "BufferIORequest__error";

	
	public static final int LOAD = 0;

	
	public static final int SAVE = 1;

	
	public static final int AUTOSAVE = 2;

	
	public static final int INSERT = 3;

	
	public static final int GZIP_MAGIC_1 = 0x1f;
	public static final int GZIP_MAGIC_2 = 0x8b;
	public static final int UNICODE_MAGIC_1 = 0xfe;
	public static final int UNICODE_MAGIC_2 = 0xff;
	public static final int UTF8_MAGIC_1 = 0xef;
	public static final int UTF8_MAGIC_2 = 0xbb;
	public static final int UTF8_MAGIC_3 = 0xbf;

	
	public static final int XML_PI_LENGTH = 50;
	

	
	
	public BufferIORequest(int type, View view, Buffer buffer,
		Object session, VFS vfs, String path)
	{
		this.type = type;
		this.view = view;
		this.buffer = buffer;
		this.session = session;
		this.vfs = vfs;
		this.path = path;

		markersPath = vfs.getParentOfPath(path)
			+ '.' + vfs.getFileName(path)
			+ ".marks";
	} 

	
	public void run()
	{
		switch(type)
		{
		case LOAD:
			load();
			break;
		case SAVE:
			save();
			break;
		case AUTOSAVE:
			autosave();
			break;
		case INSERT:
			insert();
			break;
		default:
			throw new InternalError();
		}
	} 

	
	public String toString()
	{
		String typeString;
		switch(type)
		{
		case LOAD:
			typeString = "LOAD";
			break;
		case SAVE:
			typeString = "SAVE";
			break;
		case AUTOSAVE:
			typeString = "AUTOSAVE";
			break;
		default:
			typeString = "UNKNOWN!!!";
		}

		return getClass().getName() + "[type=" + typeString
			+ ",buffer=" + buffer + "]";
	} 

	

	
	private int type;
	private View view;
	private Buffer buffer;
	private Object session;
	private VFS vfs;
	private String path;
	private String markersPath;
	

	
	private void load()
	{
		InputStream in = null;

		try
		{
			try
			{
				String[] args = { vfs.getFileName(path) };
				setAbortable(true);
				if(!buffer.isTemporary())
				{
					setStatus(jEdit.getProperty("vfs.status.load",args));
					setProgressValue(0);
				}

				path = vfs._canonPath(session,path,view);

				VFS.DirectoryEntry entry = vfs._getDirectoryEntry(
					session,path,view);
				long length;
				if(entry != null)
					length = entry.length;
				else
					length = 0L;

				in = vfs._createInputStream(session,path,
					false,view);
				if(in == null)
					return;

				read(autodetect(in),length,false);
				buffer.setNewFile(false);
			}
			catch(CharConversionException ch)
			{
				Log.log(Log.ERROR,this,ch);
				Object[] pp = { buffer.getProperty(Buffer.ENCODING),
					ch.toString() };
				VFSManager.error(view,path,"ioerror.encoding-error",pp);

				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
			catch(UnsupportedEncodingException uu)
			{
				Log.log(Log.ERROR,this,uu);
				Object[] pp = { buffer.getProperty(Buffer.ENCODING),
					uu.toString() };
				VFSManager.error(view,path,"ioerror.encoding-error",pp);

				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,this,io);
				Object[] pp = { io.toString() };
				VFSManager.error(view,path,"ioerror.read-error",pp);

				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}
			catch(OutOfMemoryError oom)
			{
				Log.log(Log.ERROR,this,oom);
				VFSManager.error(view,path,"out-of-memory-error",null);

				buffer.setBooleanProperty(ERROR_OCCURRED,true);
			}

			if(jEdit.getBooleanProperty("persistentMarkers"))
			{
				try
				{
					String[] args = { vfs.getFileName(path) };
					if(!buffer.isTemporary())
						setStatus(jEdit.getProperty("vfs.status.load-markers",args));
					setAbortable(true);

					in = vfs._createInputStream(session,markersPath,true,view);
					if(in != null)
						readMarkers(buffer,in);
				}
				catch(IOException io)
				{
					
				}
			}
		}
		catch(WorkThread.Abort a)
		{
			if(in != null)
			{
				try
				{
					in.close();
				}
				catch(IOException io)
				{
				}
			}

			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
		finally
		{
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

	
	
	private Reader autodetect(InputStream in) throws IOException
	{
		in = new BufferedInputStream(in);

		String encoding = buffer.getStringProperty(Buffer.ENCODING);
		if(!in.markSupported())
			Log.log(Log.WARNING,this,"Mark not supported: " + in);
		else if(buffer.getBooleanProperty(Buffer.ENCODING_AUTODETECT))
		{
			in.mark(XML_PI_LENGTH);
			int b1 = in.read();
			int b2 = in.read();
			int b3 = in.read();

			if(encoding.equals(MiscUtilities.UTF_8_Y))
			{
				
				
				if(b1 != UTF8_MAGIC_1 || b2 != UTF8_MAGIC_2
					|| b3 != UTF8_MAGIC_3)
				{
					
					
					
					in.reset();
				}
				else
				{
					
					
					
				}

				encoding = "UTF-8";
			}
			else if(b1 == GZIP_MAGIC_1 && b2 == GZIP_MAGIC_2)
			{
				in.reset();
				in = new GZIPInputStream(in);
				buffer.setBooleanProperty(Buffer.GZIPPED,true);
			}
			else if((b1 == UNICODE_MAGIC_1
				&& b2 == UNICODE_MAGIC_2)
				|| (b1 == UNICODE_MAGIC_2
				&& b2 == UNICODE_MAGIC_1))
			{
				in.reset();
				encoding = "UTF-16";
				buffer.setProperty(Buffer.ENCODING,encoding);
			}
			else if(b1 == UTF8_MAGIC_1 && b2 == UTF8_MAGIC_2
				&& b3 == UTF8_MAGIC_3)
			{
				
				
				buffer.setProperty(Buffer.ENCODING,
					MiscUtilities.UTF_8_Y);

				encoding = "UTF-8";
			}
			else
			{
				in.reset();

				byte[] _xmlPI = new byte[XML_PI_LENGTH];
				int offset = 0;
				int count;
				while((count = in.read(_xmlPI,offset,
					XML_PI_LENGTH - offset)) != -1)
				{
					offset += count;
					if(offset == XML_PI_LENGTH)
						break;
				}

				String xmlPI = new String(_xmlPI,0,offset,
				"ASCII");
				if(xmlPI.startsWith("<?xml"))
				{
					int index = xmlPI.indexOf("encoding=");
					if(index != -1
						&& index + 9 != xmlPI.length())
					{
						char ch = xmlPI.charAt(index
						+ 9);
						int endIndex = xmlPI.indexOf(ch,
							index + 10);
						encoding = xmlPI.substring(
							index + 10,endIndex);
	
						if(MiscUtilities.isSupportedEncoding(encoding))
						{
							buffer.setProperty(Buffer.ENCODING,encoding);
						}
						else
						{
							Log.log(Log.WARNING,this,"XML PI specifies unsupported encoding: " + encoding);
						}
					}
				}

				in.reset();
			}
		}

		return new InputStreamReader(in,encoding);
	} 

	
	private SegmentBuffer read(Reader in, long length,
		boolean insert) throws IOException
	{
		
		IntegerArray endOffsets = new IntegerArray(
			Math.max(1,(int)(length / 50)));

		
		boolean trackProgress = (!buffer.isTemporary() && length != 0);

		if(trackProgress)
		{
			setProgressValue(0);
			setProgressMaximum((int)length);
		}

		
		
		if(length == 0)
			length = IOBUFSIZE;

		SegmentBuffer seg = new SegmentBuffer((int)length + 1);

		char[] buf = new char[IOBUFSIZE];

		
		
		
		
		int len;

		
		
		boolean CRLF = false;

		
		boolean CROnly = false;

		
		
		
		boolean lastWasCR = false;

		
		
		int lineCount = 0;

		while((len = in.read(buf,0,buf.length)) != -1)
		{
			
			
			
			int lastLine = 0;

			for(int i = 0; i < len; i++)
			{
				
				switch(buf[i])
				{
				case '\r':
					
					
					
					
					if(lastWasCR)
					{
						CROnly = true;
						CRLF = false;
					}
					
					
					
					else
					{
						lastWasCR = true;
					}

					
					seg.append(buf,lastLine,i -
						lastLine);
					seg.append('\n');
					endOffsets.add(seg.count);
					if(trackProgress && lineCount++ % PROGRESS_INTERVAL == 0)
						setProgressValue(seg.count);

					
					
					lastLine = i + 1;
					break;
				case '\n':
					
					
					
					
					
					
					
					if(lastWasCR)
					{
						CROnly = false;
						CRLF = true;
						lastWasCR = false;
						
						
						
						
						lastLine = i + 1;
					}
					
					
					
					
					else
					{
						CROnly = false;
						CRLF = false;
						seg.append(buf,lastLine,
							i - lastLine);
						seg.append('\n');
						endOffsets.add(seg.count);
						if(trackProgress && lineCount++ % PROGRESS_INTERVAL == 0)
							setProgressValue(seg.count);
						lastLine = i + 1;
					}
					break;
				default:
					
					
					
					
					
					if(lastWasCR)
					{
						CROnly = true;
						CRLF = false;
						lastWasCR = false;
					}
					break;
				}
			}

			if(trackProgress)
				setProgressValue(seg.count);

			
			seg.append(buf,lastLine,len - lastLine);
		}

		setAbortable(false);

		String lineSeparator;
		if(length == 0)
		{
			
			
			lineSeparator = null;
		}
		else if(CRLF)
			lineSeparator = "\r\n";
		else if(CROnly)
			lineSeparator = "\r";
		else
			lineSeparator = "\n";

		in.close();

		
		int bufferLength = seg.count;
		if(bufferLength != 0)
		{
			char ch = seg.array[bufferLength - 1];
			if(ch == 0x1a )
				seg.count--;
		}

		buffer.setBooleanProperty(Buffer.TRAILING_EOL,false);
		if(bufferLength != 0 && jEdit.getBooleanProperty("stripTrailingEOL"))
		{
			char ch = seg.array[bufferLength - 1];
			if(ch == '\n')
			{
				buffer.setBooleanProperty(Buffer.TRAILING_EOL,true);
				seg.count--;
				endOffsets.setSize(endOffsets.getSize() - 1);
			}
		}

		
		
		endOffsets.add(seg.count + 1);

		
		
		
		if(!insert)
		{
			buffer.setProperty(LOAD_DATA,seg);
			buffer.setProperty(END_OFFSETS,endOffsets);
			buffer.setProperty(NEW_PATH,path);
			if(lineSeparator != null)
				buffer.setProperty(Buffer.LINESEP,lineSeparator);
		}

		
		return seg;
	} 

	
	private void readMarkers(Buffer buffer, InputStream _in)
		throws IOException
	{
		
		buffer.removeAllMarkers();

		BufferedReader in = new BufferedReader(new InputStreamReader(_in));

		String line;
		while((line = in.readLine()) != null)
		{
			
			if(!line.startsWith("!"))
				continue;

			char shortcut = line.charAt(1);
			int start = line.indexOf(';');
			int end = line.indexOf(';',start + 1);
			int position = Integer.parseInt(line.substring(start + 1,end));
			buffer.addMarker(shortcut,position);
		}

		in.close();
	} 

	
	private void save()
	{
		OutputStream out = null;

		try
		{
			String[] args = { vfs.getFileName(path) };
			setStatus(jEdit.getProperty("vfs.status.save",args));

			
			setAbortable(true);

			path = vfs._canonPath(session,path,view);			if(!MiscUtilities.isURL(path))
				path = MiscUtilities.resolveSymlinks(path);

			
			if(buffer.getProperty(Buffer.BACKED_UP) == null
				|| jEdit.getBooleanProperty("backupEverySave"))
			{
				vfs._backup(session,path,view);
				buffer.setBooleanProperty(Buffer.BACKED_UP,true);
			}

			
			String savePath;

			boolean twoStageSave = (vfs.getCapabilities() & VFS.RENAME_CAP) != 0
				&& jEdit.getBooleanProperty("twoStageSave");
			if(twoStageSave)
				savePath = vfs.getTwoStageSaveName(path);
			else
				savePath = path;

			out = vfs._createOutputStream(session,savePath,view);

			try
			{
				
				
				buffer.readLock();
				if(out != null)
				{
					
					
					
					if(savePath.endsWith(".gz"))
						buffer.setBooleanProperty(Buffer.GZIPPED,true);

					if(buffer.getBooleanProperty(Buffer.GZIPPED))
						out = new GZIPOutputStream(out);

					write(buffer,out);

					if(twoStageSave)
					{
						if(!vfs._rename(session,savePath,path,view))
							throw new IOException("Rename failed: " + savePath);
					}

					
					
					if((vfs.getCapabilities() & VFS.DELETE_CAP) != 0)
					{
						if(jEdit.getBooleanProperty("persistentMarkers")
							&& buffer.getMarkers().size() != 0)
						{
							setStatus(jEdit.getProperty("vfs.status.save-markers",args));
							setProgressValue(0);
							out = vfs._createOutputStream(session,markersPath,view);
							if(out != null)
								writeMarkers(buffer,out);
						}
						else
							vfs._delete(session,markersPath,view);
					}
				}
				else
					buffer.setBooleanProperty(ERROR_OCCURRED,true);

				if(!twoStageSave)
					VFSManager.sendVFSUpdate(vfs,path,true);
			}
			finally
			{
				buffer.readUnlock();
			}
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
		finally
		{
			try
			{
				vfs._saveComplete(session,buffer,path,view);
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

	
	private void autosave()
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
			finally
			{
				
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
		}
	} 

	
	private void write(Buffer buffer, OutputStream _out)
		throws IOException
	{
		String encoding = buffer.getStringProperty(Buffer.ENCODING);
		if(encoding.equals(MiscUtilities.UTF_8_Y))
		{
			
			_out.write(UTF8_MAGIC_1);
			_out.write(UTF8_MAGIC_2);
			_out.write(UTF8_MAGIC_3);
			_out.flush();
			encoding = "UTF-8";
		}

		BufferedWriter out = new BufferedWriter(
			new OutputStreamWriter(_out,encoding),
			IOBUFSIZE);

		Segment lineSegment = new Segment();
		String newline = buffer.getStringProperty(Buffer.LINESEP);
		if(newline == null)
			newline = System.getProperty("line.separator");

		setProgressMaximum(buffer.getLineCount() / PROGRESS_INTERVAL);
		setProgressValue(0);

		int i = 0;
		while(i < buffer.getLineCount())
		{
			buffer.getLineText(i,lineSegment);
			out.write(lineSegment.array,lineSegment.offset,
				lineSegment.count);

			if(i != buffer.getLineCount() - 1)
			{
				out.write(newline);
			}

			if(++i % PROGRESS_INTERVAL == 0)
				setProgressValue(i / PROGRESS_INTERVAL);
		}

		if(jEdit.getBooleanProperty("stripTrailingEOL")
			&& buffer.getBooleanProperty(Buffer.TRAILING_EOL))
		{
			out.write(newline);
		}

		out.close();
	} 

	
	private void writeMarkers(Buffer buffer, OutputStream out)
		throws IOException
	{
		Writer o = new BufferedWriter(new OutputStreamWriter(out));
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
		o.close();
	} 

	
	private void insert()
	{
		InputStream in = null;

		try
		{
			try
			{
				String[] args = { vfs.getFileName(path) };
				setStatus(jEdit.getProperty("vfs.status.load",args));
				setAbortable(true);

				path = vfs._canonPath(session,path,view);

				VFS.DirectoryEntry entry = vfs._getDirectoryEntry(
					session,path,view);
				long length;
				if(entry != null)
					length = entry.length;
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
		}
		catch(WorkThread.Abort a)
		{
			if(in != null)
			{
				try
				{
					in.close();
				}
				catch(IOException io)
				{
				}
			}

			buffer.setBooleanProperty(ERROR_OCCURRED,true);
		}
		finally
		{
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
