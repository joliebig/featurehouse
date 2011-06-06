

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.*;


public class EditPaneUpdate extends EBMessage
{
	
	public static final Object CREATED = "CREATED";

	
	public static final Object DESTROYED = "DESTROYED";

	
	public static final Object BUFFER_CHANGED = "BUFFER_CHANGED";

	
	public static final Object BUFFER_CHANGING = "BUFFER_CHANGING";
	
	
	public EditPaneUpdate(EditPane editPane, Object what)
	{
		super(editPane);
		if(what == null)
			throw new NullPointerException("What must be non-null");

		this.what = what;
	}

	
	public Object getWhat()
	{
		return what;
	}

	
	public EditPane getEditPane()
	{
		return (EditPane)getSource();
	}

	public String paramString()
	{
		return "what=" + what + "," + super.paramString();
	}
	
	
	private Object what;

}
