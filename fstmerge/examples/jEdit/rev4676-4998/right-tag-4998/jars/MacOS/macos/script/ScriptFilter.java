

package macos.script;


import java.io.*;


public class ScriptFilter implements FileFilter
{
	
	public boolean accept(File pathname)
	{
		if (pathname.getName().endsWith(".scpt"))
			return true;
		if (pathname.getName().endsWith(".applescript"))
			return true;
		
		
		
		
		return false;
	} 
}
