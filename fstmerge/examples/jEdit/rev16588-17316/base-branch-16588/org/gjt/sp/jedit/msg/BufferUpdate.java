

package org.gjt.sp.jedit.msg;

import org.gjt.sp.jedit.*;


public class BufferUpdate extends EBMessage
{
	
	
	public static final Object CREATED = "CREATED";
	
	
	public static final Object CLOSING = "CLOSING";
	
	public static final Object LOAD_STARTED = "LOAD_STARTED";

	
	public static final Object LOADED = "LOADED";

	
	public static final Object CLOSED = "CLOSED";

	
	public static final Object DIRTY_CHANGED = "DIRTY_CHANGED";

	
	public static final Object MARKERS_CHANGED = "MARKERS_CHANGED";

	
	public static final Object SAVING = "SAVING";

	
	public static final Object SAVED = "SAVED";

	
	public static final Object PROPERTIES_CHANGED = "PROPERTIES_CHANGED";
	

	
	
	public BufferUpdate(Buffer buffer, View view, Object what)
	{
		super(buffer);

		this.view = view;

		if(what == null)
			throw new NullPointerException("What must be non-null");

		this.what = what;
	} 

	
	
	public Object getWhat()
	{
		return what;
	} 

	
	
	public Buffer getBuffer()
	{
		return (Buffer)getSource();
	} 

	
	
	public View getView()
	{
		return view;
	} 

	
	public String paramString()
	{
		return "what=" + what + ",view=" + view + ","
			+ super.paramString();
	} 

	
	private Object what;
	private View view;
	
}
