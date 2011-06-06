

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.*;


public class VFSUpdate extends EBMessage
{
	
	public VFSUpdate(String path)
	{
		super(null);

		if(path == null)
			throw new NullPointerException("Path must be non-null");

		this.path = path;
	}

	
	public String getPath()
	{
		return path;
	}

	public String paramString()
	{
		return "path=" + path + "," + super.paramString();
	}

	
	private String path;
}
