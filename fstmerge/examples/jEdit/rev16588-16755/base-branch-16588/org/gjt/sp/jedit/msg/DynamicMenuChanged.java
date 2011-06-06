

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.*;


public class DynamicMenuChanged extends EBMessage
{
	
	
	public DynamicMenuChanged(String name)
	{
		super(null);

		this.name = name;
	} 

	
	
	public String getMenuName()
	{
		return name;
	} 

	
	public String paramString()
	{
		return "menu=" + name + "," + super.paramString();
	} 

	
	private String name;
	
}
