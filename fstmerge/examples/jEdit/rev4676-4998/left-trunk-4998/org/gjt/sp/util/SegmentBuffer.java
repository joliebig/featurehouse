

package org.gjt.sp.util;

import javax.swing.text.Segment;


public class SegmentBuffer extends Segment
{
	
	public SegmentBuffer(int capacity)
	{
		ensureCapacity(capacity);
	} 

	
	public void append(char ch)
	{
		ensureCapacity(count + 1);
		array[offset + count] = ch;
		count++;
	} 

	
	public void append(char[] text, int off, int len)
	{
		ensureCapacity(count + len);
		System.arraycopy(text,off,array,count,len);
		count += len;
	} 

	

	
	private void ensureCapacity(int capacity)
	{
		if(array == null)
			array = new char[capacity];
		else if(capacity >= array.length)
		{
			char[] arrayN = new char[capacity * 2];
			System.arraycopy(array,0,arrayN,0,count);
			array = arrayN;
		}
	} 

	
}
