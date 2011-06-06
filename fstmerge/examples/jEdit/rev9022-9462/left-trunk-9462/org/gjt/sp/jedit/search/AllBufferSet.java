

package org.gjt.sp.jedit.search;


import java.awt.Component;
import java.util.ArrayList;
import java.util.regex.Pattern;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;



public class AllBufferSet extends BufferListSet
{
	
	
	public AllBufferSet(String glob)
	{
		this.glob = glob;
	} 

	
	
	public String getFileFilter()
	{
		return glob;
	} 

	
	
	public String getCode()
	{
		return "new AllBufferSet(\"" + MiscUtilities.charsToEscapes(glob)
			+ "\")";
	} 

	
	private String glob;
	

	
	protected String[] _getFiles(Component comp)
	{
		Buffer[] buffers = jEdit.getBuffers();
		ArrayList returnValue = new ArrayList(buffers.length);

		Pattern filter;
		try
		{
			filter = Pattern.compile(StandardUtilities.globToRE(glob));
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,e);
			return null;
		}

		for(int i = 0; i < buffers.length; i++)
		{
			Buffer buffer = buffers[i];
			if(filter.matcher(buffer.getName()).matches())
				returnValue.add(buffer.getPath());
		}

		return (String[])returnValue.toArray(new String[returnValue.size()]);
	} 
}
