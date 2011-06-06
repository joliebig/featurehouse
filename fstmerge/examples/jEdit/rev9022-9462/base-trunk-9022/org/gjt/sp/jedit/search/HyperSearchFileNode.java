

package org.gjt.sp.jedit.search;

import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.EditPane;
import org.gjt.sp.jedit.MiscUtilities;
import org.gjt.sp.jedit.jEdit;


public class HyperSearchFileNode implements HyperSearchNode
{
	public String path;
	public Buffer buffer;
	public boolean showFullPath = true;

	private static String fileSep = System.getProperty("file.separator");
	static
	{
		if (fileSep.equals("\\"))
			fileSep = "\\\\";
	}

	
	public HyperSearchFileNode(String path)
	{
		this.path = path;
	} 

	
	public Buffer getBuffer()
	{
		if(buffer == null)
			buffer = jEdit.openFile(null,path);
		return buffer;
	} 

	
	public void goTo(EditPane editPane)
	{
		Buffer buffer = getBuffer();
		if(buffer == null)
			return;

		editPane.setBuffer(buffer);
	} 
	
	
	public String toString()
	{
		if (showFullPath)
			return path;
		String[] paths = path.split(fileSep);
		return paths[paths.length - 1];
	} 
	
	
	public boolean equals(Object compareObj)
	{
		if (!(compareObj instanceof HyperSearchFileNode))
			return false;
		HyperSearchFileNode otherResult = (HyperSearchFileNode)compareObj;
		
		return path.equals(MiscUtilities.resolveSymlinks(otherResult.path))
			&& buffer.equals(otherResult.buffer);		
	}

	
	public int getCount()
	{
		return count;
	}

	
	public void setCount(int count)
	{
		this.count = count;
	}

	private int count;
}
