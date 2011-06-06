

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.*;


public class PluginUpdate extends EBMessage
{
	
	
	public static final Object LOADED = "LOADED";

	
	public static final Object ACTIVATED = "ACTIVATED";

	
	public static final Object UNLOADED = "UNLOADED";
	

	
	
	public PluginUpdate(PluginJAR jar, Object what)
	{
		super(jar);

		if(what == null)
			throw new NullPointerException("What must be non-null");

		this.what = what;
	} 

	
	
	public Object getWhat()
	{
		return what;
	} 

	
	
	public PluginJAR getPluginJAR()
	{
		return (PluginJAR)getSource();
	} 

	
	public String paramString()
	{
		return "what=" + what + ","
			+ super.paramString();
	} 

	
	private Object what;
	
}
