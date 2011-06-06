

package org.gjt.sp.jedit;


import gnu.regexp.*;
import java.util.Hashtable;
import org.gjt.sp.jedit.syntax.TokenMarker;
import org.gjt.sp.util.Log;



public class Mode
{
	
	
	public Mode(String name)
	{
		this.name = name;
		props = new Hashtable();
	} 

	
	
	public void init()
	{
		try
		{
			String filenameGlob = (String)getProperty("filenameGlob");
			if(filenameGlob != null && filenameGlob.length() != 0)
			{
				filenameRE = new RE(MiscUtilities.globToRE(
					filenameGlob),RE.REG_ICASE);
			}

			String firstlineGlob = (String)getProperty("firstlineGlob");
			if(firstlineGlob != null && firstlineGlob.length() != 0)
			{
				firstlineRE = new RE(MiscUtilities.globToRE(
					firstlineGlob),RE.REG_ICASE);
			}
		}
		catch(REException re)
		{
			Log.log(Log.ERROR,this,"Invalid filename/firstline"
				+ " globs in mode " + name);
			Log.log(Log.ERROR,this,re);
		}

		
		
		
		
		
		
		marker = null;
	} 

	
	
	public TokenMarker getTokenMarker()
	{
		loadIfNecessary();
		return marker;
	} 

	
	
	public void setTokenMarker(TokenMarker marker)
	{
		this.marker = marker;
	} 

	
	
	public void loadIfNecessary()
	{
		if(marker == null)
			jEdit.loadMode(this);
	} 

	
	
	public Object getProperty(String key)
	{
		String prefix = "mode." + name + ".";

		
		
			String property = jEdit.getProperty(prefix + key);
			if(property != null)
			{
				Object value;
				try
				{
					value = new Integer(property);
				}
				catch(NumberFormatException nf)
				{
					value = property;
				}
				return value;
			}
		

		Object value = props.get(key);
		if(value != null)
			return value;

		String global = jEdit.getProperty("buffer." + key);
		if(global != null)
		{
			try
			{
				return new Integer(global);
			}
			catch(NumberFormatException nf)
			{
				return global;
			}
		}
		else
			return null;
	} 

	
	
	public boolean getBooleanProperty(String key)
	{
		Object value = getProperty(key);
		if("true".equals(value) || "on".equals(value) || "yes".equals(value))
			return true;
		else
			return false;
	} 

	
	
	public void setProperty(String key, Object value)
	{
		props.put(key,value);
	} 

	
	
	public void unsetProperty(String key)
	{
		props.remove(key);
	} 

	
	
	public void setProperties(Hashtable props)
	{
		if(props == null)
			props = new Hashtable();

		
		
		
		String filenameGlob = (String)this.props.get("filenameGlob");
		String firstlineGlob = (String)this.props.get("firstlineGlob");
		String filename = (String)this.props.get("file");
		this.props = props;
		if(filenameGlob != null)
			props.put("filenameGlob",filenameGlob);
		if(firstlineGlob != null)
			props.put("firstlineGlob",firstlineGlob);
		if(filename != null)
			props.put("file",filename);
	} 

	
	
	public boolean accept(String fileName, String firstLine)
	{
		if(filenameRE != null && filenameRE.isMatch(fileName))
			return true;

		if(firstlineRE != null && firstlineRE.isMatch(firstLine))
			return true;

		return false;
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public String toString()
	{
		return name;
	} 

	
	private String name;
	private Hashtable props;
	private RE firstlineRE;
	private RE filenameRE;
	private TokenMarker marker;
	
}
