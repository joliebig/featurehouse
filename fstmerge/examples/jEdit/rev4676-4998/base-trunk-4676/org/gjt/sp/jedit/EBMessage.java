

package org.gjt.sp.jedit;


public abstract class EBMessage
{
	
	
	public EBMessage(Object source)
	{
		this.source = source;
	} 

	
	
	public Object getSource()
	{
		return source;
	} 

	
	
	public String toString()
	{
		String className = getClass().getName();
		int index = className.lastIndexOf('.');
		return className.substring(index + 1)
			+ "[" + paramString() + "]";
	} 

	
	
	public String paramString()
	{
		return "source=" + source;
	} 

	
	private Object source;
	

	
	
	public void veto()
	{
	}

	
	public boolean isVetoed()
	{
		return false;
	}

	
	public static abstract class NonVetoable extends EBMessage
	{
		
		public NonVetoable(EBComponent source)
		{
			super(source);
		}

		
		public void veto()
		{
			throw new InternalError("Can't veto this message");
		}
	} 
}
