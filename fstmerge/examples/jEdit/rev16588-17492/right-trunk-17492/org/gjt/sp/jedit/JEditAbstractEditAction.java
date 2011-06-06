

package org.gjt.sp.jedit;


public abstract class JEditAbstractEditAction<E>
{
	
	protected String name;

	protected Object[] args;

	

	
	
	protected JEditAbstractEditAction(String name)
	{
		this.name = name;
	}

	protected JEditAbstractEditAction(String name, Object[] newArgs)
	{
		this.name = name;
		this.args = newArgs;
	} 

	
	
	public String getName()
	{
		return name;
	} 

	
	
	public void setName(String newName)
	{
		name = newName;
	}

	
	
	public abstract void invoke(E arg);

	
	public final void invoke(E arg, Object[] newArgs)
	{
		args = newArgs;
		invoke(arg);
	} 

	
	@Override
	public String toString()
	{
		return name;
	} 
}
