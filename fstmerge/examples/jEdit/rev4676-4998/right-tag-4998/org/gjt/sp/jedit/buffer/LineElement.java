

package org.gjt.sp.jedit.buffer;


import javax.swing.text.*;
import org.gjt.sp.jedit.Buffer;



public class LineElement implements Element
{
	
	public LineElement(Buffer buffer, int line)
	{
		this.buffer = buffer;
		this.line = line;
	} 

	
	public Document getDocument()
	{
		return null;
	} 

	
	public Element getParentElement()
	{
		return null;
	} 

	
	public String getName()
	{
		return null;
	} 

	
	public AttributeSet getAttributes()
	{
		return null;
	} 

	
	public int getStartOffset()
	{
		return buffer.getLineStartOffset(line);
	} 

	
	public int getEndOffset()
	{
		return buffer.getLineEndOffset(line);
	} 

	
	public int getElementIndex(int offset)
	{
		return 0;
	} 

	
	public int getElementCount()
	{
		return 0;
	} 

	
	public Element getElement(int line)
	{
		return null;
	} 

	
	public boolean isLeaf()
	{
		return true;
	} 

	
	private Buffer buffer;
	private int line;
	
} 
