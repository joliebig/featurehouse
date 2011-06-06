

package org.gjt.sp.jedit.io;


import java.awt.Component;
import java.io.*;
import java.net.*;
import org.gjt.sp.util.Log;



public class UrlVFS extends VFS
{
	
	public UrlVFS()
	{
		super("url",READ_CAP | WRITE_CAP);
	} 

	
	public String constructPath(String parent, String path)
	{
		if(parent.endsWith("/"))
			return parent + path;
		else
			return parent + '/' + path;
	} 

	
	public InputStream _createInputStream(Object session,
		String path, boolean ignoreErrors, Component comp)
		throws IOException
	{
		try
		{
			return new URL(path).openStream();
		}
		catch(MalformedURLException mu)
		{
			Log.log(Log.ERROR,this,mu);
			String[] args = { mu.getMessage() };
			VFSManager.error(comp,path,"ioerror.badurl",args);
			return null;
		}
	} 

	
	public OutputStream _createOutputStream(Object session, String path,
		Component comp) throws IOException
	{
		try
		{
			return new URL(path).openConnection()
				.getOutputStream();
		}
		catch(MalformedURLException mu)
		{
			Log.log(Log.ERROR,this,mu);
			String[] args = { mu.getMessage() };
			VFSManager.error(comp,path,"ioerror.badurl",args);
			return null;
		}
	} 
}
