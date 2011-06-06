

package org.gjt.sp.jedit.bufferio;


import java.io.BufferedOutputStream;
import java.io.CharConversionException;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.Writer;
import java.nio.charset.CharacterCodingException;

import javax.swing.text.Segment;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.View;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.io.VFS;
import org.gjt.sp.jedit.io.EncodingServer;
import org.gjt.sp.util.IntegerArray;
import org.gjt.sp.util.SegmentBuffer;
import org.gjt.sp.util.WorkRequest;



public abstract class BufferIORequest extends WorkRequest
{
	

	
	public static final int GZIP_MAGIC_1 = 0x1f;
	public static final int GZIP_MAGIC_2 = 0x8b;

	
	public static final int XML_PI_LENGTH = 50;

	
	public static final int IOBUFSIZE = 32768;

	
	public static final int PROGRESS_INTERVAL = 300;

	public static final String LOAD_DATA = "BufferIORequest__loadData";
	public static final String END_OFFSETS = "BufferIORequest__endOffsets";
	public static final String NEW_PATH = "BufferIORequest__newPath";

	
	public static final String ERROR_OCCURRED = "BufferIORequest__error";

	
	@Deprecated public static final int UTF8_MAGIC_1 = 0xef;
	@Deprecated public static final int UTF8_MAGIC_2 = 0xbb;
	@Deprecated public static final int UTF8_MAGIC_3 = 0xbf;
	@Deprecated public static final int UNICODE_MAGIC_1 = 0xfe;
	@Deprecated public static final int UNICODE_MAGIC_2 = 0xff;

	

	
	protected final View view;
	protected final Buffer buffer;
	protected final Object session;
	protected final VFS vfs;
	protected String path;
	protected final String markersPath;
	

	

	
	
	protected BufferIORequest(View view, Buffer buffer,
		Object session, VFS vfs, String path)
	{
		this.view = view;
		this.buffer = buffer;
		this.session = session;
		this.vfs = vfs;
		this.path = path;

		markersPath = Buffer.getMarkersPath(vfs, path);
	} 

	
	public String toString()
	{
		return getClass().getName() + '[' + buffer + ']';
	} 


	
	public static int getCharIOBufferSize()
	{
		return IOBUFSIZE;
	}

	
	public static int getByteIOBufferSize()
	{
		
		return IOBUFSIZE * 2;
	}

	
	
	protected Reader autodetect(InputStream in) throws IOException
	{
		return MiscUtilities.autodetect(in, buffer);
	} 

	
	protected SegmentBuffer read(Reader in, long length,
		boolean insert) throws IOException
	{
		
		IntegerArray endOffsets = new IntegerArray(
			Math.max(1,(int)(length / 50)));

		
		boolean trackProgress = !buffer.isTemporary() && length != 0;

		if(trackProgress)
		{
			setMaximum(length);
			setValue(0);
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
						setValue(seg.count);

					
					
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
							setValue(seg.count);
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
				setValue(seg.count);

			
			seg.append(buf,lastLine,len - lastLine);
		}

		setAbortable(false);

		String lineSeparator;
		if(seg.count == 0)
		{
			
			
			lineSeparator = jEdit.getProperty(
				"buffer.lineSeparator",
				System.getProperty("line.separator"));
		}
		else if(CRLF)
			lineSeparator = "\r\n";
		else if(CROnly)
			lineSeparator = "\r";
		else
			lineSeparator = "\n";

		
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
				buffer.setProperty(JEditBuffer.LINESEP,lineSeparator);
		}

		
		return seg;
	} 

	
	protected void write(Buffer buffer, OutputStream out)
		throws IOException
	{
		Writer writer = EncodingServer.getTextWriter(
			new BufferedOutputStream(out, getByteIOBufferSize()),
			buffer.getStringProperty(JEditBuffer.ENCODING));

		Segment lineSegment = new Segment();
		String newline = buffer.getStringProperty(JEditBuffer.LINESEP);
		if(newline == null)
			newline = System.getProperty("line.separator");

		final int bufferLineCount = buffer.getLineCount();
		setMaximum(bufferLineCount / PROGRESS_INTERVAL);
		setValue(0);

		int i = 0;
		while(i < bufferLineCount)
		{
			buffer.getLineText(i,lineSegment);
			try
			{
				writer.write(lineSegment.array,
					lineSegment.offset,
					lineSegment.count);
				if(i < bufferLineCount - 1
					|| (jEdit.getBooleanProperty("stripTrailingEOL")
						&& buffer.getBooleanProperty(Buffer.TRAILING_EOL)))
				{
					writer.write(newline);
				}
			}
			catch(CharacterCodingException e)
			{
				String message = "Failed to encode the line " + (i + 1);
				IOException wrapping = new CharConversionException(message);
				wrapping.initCause(e);
				throw wrapping;
			}

			if(++i % PROGRESS_INTERVAL == 0)
				setValue(i / PROGRESS_INTERVAL);
		}
		writer.flush();
	} 
	

}

