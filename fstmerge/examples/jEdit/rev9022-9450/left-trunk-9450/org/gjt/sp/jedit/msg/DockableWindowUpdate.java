

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.gui.DockableWindowManager;
import org.gjt.sp.jedit.*;


public class DockableWindowUpdate extends EBMessage
{
	
	
	public static final Object PROPERTIES_CHANGED = "PROPERTIES_CHANGED";

	
	public static final Object ACTIVATED = "ACTIVATED";

	
	public static final Object DEACTIVATED = "DEACTIVATED";
	

	
	
	public DockableWindowUpdate(DockableWindowManager wm, Object what,
		String dockable)
	{
		super(wm);

		if(what == null)
			throw new NullPointerException("What must be non-null");

		this.what = what;
		this.dockable = dockable;
	} 

	
	
	public Object getWhat()
	{
		return what;
	} 

	
	
	public String getDockable()
	{
		return dockable;
	} 

	
	public String paramString()
	{
		return "what=" + what
			+ ",dockable=" + dockable
			+ "," + super.paramString();
	} 

	
	private Object what;
	private String dockable;
	
}
