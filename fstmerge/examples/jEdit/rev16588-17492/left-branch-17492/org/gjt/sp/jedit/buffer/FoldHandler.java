

package org.gjt.sp.jedit.buffer;

import java.util.List;

import javax.swing.text.Segment;


public abstract class FoldHandler
{
	
	@Deprecated
	public static final String SERVICE = "org.gjt.sp.jedit.buffer.FoldHandler";

	
	public static FoldHandlerProvider foldHandlerProvider;

	
	
	public String getName()
	{
		return name;
	}
	

	
	
	public abstract int getFoldLevel(JEditBuffer buffer, int lineIndex, Segment seg);
	

	
	
	public List<Integer> getPrecedingFoldLevels(JEditBuffer buffer,
		int lineIndex, Segment seg, int lineFoldLevel)
	{
		return null;
	}
	

	
	
	public boolean equals(Object o)
	{
		
		if(o == null)
			return false;
		else
			return getClass() == o.getClass();
	} 

	
	public int hashCode()
	{
		return getClass().hashCode();
	} 

	
	
	public static FoldHandler getFoldHandler(String name)
	{
		return foldHandlerProvider.getFoldHandler(name);
	}
	

	
	
	public static String[] getFoldModes()
	{
		return foldHandlerProvider.getFoldModes();
	}
	

	
	protected FoldHandler(String name)
	{
		this.name = name;
	}
	

	
	public String toString()
	{
		return name;
	} 

	private String name;
}
