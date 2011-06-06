

package org.gjt.sp.jedit.search;

import org.gjt.sp.jedit.*;


public class CurrentBufferSet implements SearchFileSet
{
	
	public String getFirstFile(View view)
	{
		return view.getBuffer().getPath();
	} 

	
	public String getNextFile(View view, String file)
	{
		if(file == null)
			return view.getBuffer().getPath();
		else
			return null;
	} 

	
	public String[] getFiles(View view)
	{
		return new String[] { view.getBuffer().getPath() };
	} 

	
	public int getFileCount(View view)
	{
		return 1;
	} 

	
	public String getCode()
	{
		return "new CurrentBufferSet()";
	} 
}
