

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.*;


public class ViewUpdate extends EBMessage
{
	
	public static final Object CREATED = "CREATED";

	
	public static final Object CLOSED = "CLOSED";

	
	public static final Object EDIT_PANE_CHANGED = "EDIT_PANE_CHANGED";

	
	public static final Object ACTIVATED = "VIEW_ACTIVATED";

	
	
	public ViewUpdate(View view, Object what)
	{
		super(view);

		if(what == null)
			throw new NullPointerException("What must be non-null");

		this.what = what;
	} 

	
	
	public Object getWhat()
	{
		return what;
	} 

	
	
	public View getView()
	{
		return (View)getSource();
	} 

	
	public String paramString()
	{
		return "what=" + what + "," + super.paramString();
	} 

	
	private Object what;
	
}
