

package org.gjt.sp.jedit.search;


import gnu.regexp.*;
import java.awt.Component;
import java.util.ArrayList;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



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

		RE filter;
		try
		{
			filter = new RE(MiscUtilities.globToRE(glob));
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,this,e);
			return null;
		}

		for(int i = 0; i < buffers.length; i++)
		{
			Buffer buffer = buffers[i];
			if(filter.isMatch(buffer.getName()))
				returnValue.add(buffer.getPath());
		}

		return (String[])returnValue.toArray(new String[returnValue.size()]);
	} 
}
