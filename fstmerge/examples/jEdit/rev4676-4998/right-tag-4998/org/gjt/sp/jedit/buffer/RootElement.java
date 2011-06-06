

package org.gjt.sp.jedit.buffer;


import javax.swing.text.*;
import org.gjt.sp.jedit.Buffer;



public class RootElement implements Element
{
	
	public RootElement(Buffer buffer)
	{
		this.buffer = buffer;
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
		return 0;
	} 

	
	public int getEndOffset()
	{
		return buffer.getLength() + 1;
	} 

	
	public int getElementIndex(int offset)
	{
		return buffer.getLineOfOffset(offset);
	} 

	
	public int getElementCount()
	{
		return buffer.getLineCount();
	} 

	
	public Element getElement(int line)
	{
		return new LineElement(buffer,line);
	} 

	
	public boolean isLeaf()
	{
		return false;
	} 

	
	private Buffer buffer;
	
} 
