

package org.gjt.sp.jedit.search;


import javax.swing.text.*;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.Buffer;



public class HyperSearchResult
{
	public String path;
	public Buffer buffer;
	public int line;
	public int start;
	public int end;
	public Position startPos;
	public Position endPos;
	public String str; 

	
	public HyperSearchResult(Buffer buffer, int line, int start, int end)
	{
		path = buffer.getPath();
		this.line = line;
		this.start = start;
		this.end = end;

		if(!buffer.isTemporary())
			bufferOpened(buffer);

		str = (line + 1) + ": " + buffer.getLineText(line)
			.replace('\t',' ').trim();
	} 

	
	public void bufferOpened(Buffer buffer)
	{
		this.buffer = buffer;
		startPos = buffer.createPosition(Math.min(buffer.getLength(),start));
		endPos = buffer.createPosition(Math.min(buffer.getLength(),end));
	} 

	
	public void bufferClosed()
	{
		buffer = null;
		start = startPos.getOffset();
		end = endPos.getOffset();
		startPos = endPos = null;
	} 

	
	public Buffer getBuffer()
	{
		if(buffer == null)
			buffer = jEdit.openFile(null,path);
		return buffer;
	} 

	
	public String toString()
	{
		return str;
	} 
}
