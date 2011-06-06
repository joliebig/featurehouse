package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.EBMessage;
import org.gjt.sp.jedit.View;


public class VFSPathSelected extends EBMessage
{
	
	public VFSPathSelected(View source, String path, boolean isDirectory)
	{
		super(source);
		this.path = path;
		this.isDir = isDirectory;
	}

	public View getView()
	{
		return (View) getSource();
	}

	
	public String getPath()
	{
		return path;
	}

	
	public boolean isDirectory()
	{
		return isDir;
	}

	private final String path;
	private boolean isDir;
}

