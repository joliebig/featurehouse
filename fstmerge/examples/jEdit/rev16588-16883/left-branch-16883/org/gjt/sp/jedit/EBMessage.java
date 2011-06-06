

package org.gjt.sp.jedit;


public abstract class EBMessage
{
	
	
	protected EBMessage(Object source)
	{
		this.source = source;
	} 

	
	
	protected EBMessage(EBComponent source)
	{
		this.source = source;
	} 

	
	
	public Object getSource()
	{
		return source;
	} 

	
	
	@Override
	public String toString()
	{
		String className = getClass().getName();
		int index = className.lastIndexOf('.');
		return className.substring(index + 1)
			+ '[' + paramString() + ']';
	} 

	
	
	public String paramString()
	{
		return "source=" + source;
	} 

	
	private Object source;
	
}
