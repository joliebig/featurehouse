

package org.gjt.sp.jedit.buffer;

import javax.swing.text.Segment;


public class ContentManager
{
	
	public final int getLength()
	{
		return length;
	} 

	
	public String getText(int start, int len)
	{
		if(start >= gapStart)
			return new String(text,start + gapEnd - gapStart,len);
		else if(start + len <= gapStart)
			return new String(text,start,len);
		else
		{
			return new String(text,start,gapStart - start)
				.concat(new String(text,gapEnd,start + len - gapStart));
		}
	} 

	
	public void getText(int start, int len, Segment seg)
	{
		if(start >= gapStart)
		{
			seg.array = text;
			seg.offset = start + gapEnd - gapStart;
			seg.count = len;
		}
		else if(start + len <= gapStart)
		{
			seg.array = text;
			seg.offset = start;
			seg.count = len;
		}
		else
		{
			seg.array = new char[len];

			
			System.arraycopy(text,start,seg.array,0,gapStart - start);

			
			System.arraycopy(text,gapEnd,seg.array,gapStart - start,
				len + start - gapStart);

			seg.offset = 0;
			seg.count = len;
		}
	} 

	
	public void insert(int start, String str)
	{
		int len = str.length();
		moveGapStart(start);
		if(gapEnd - gapStart < len)
		{
			ensureCapacity(length + len + 1024);
			moveGapEnd(start + len + 1024);
		}

		str.getChars(0,len,text,start);
		gapStart += len;
		length += len;
	} 

	
	public void insert(int start, Segment seg)
	{
		moveGapStart(start);
		if(gapEnd - gapStart < seg.count)
		{
			ensureCapacity(length + seg.count + 1024);
			moveGapEnd(start + seg.count + 1024);
		}

		System.arraycopy(seg.array,seg.offset,text,start,seg.count);
		gapStart += seg.count;
		length += seg.count;
	} 

	
	public void _setContent(char[] text, int length)
	{
		this.text = text;
		this.gapStart = this.gapEnd = 0;
		this.length = length;
	} 

	
	public void remove(int start, int len)
	{
		moveGapStart(start);
		gapEnd += len;
		length -= len;
	} 

	
	private char[] text;
	private int gapStart;
	private int gapEnd;
	private int length;

	
	private void moveGapStart(int newStart)
	{
		int newEnd = gapEnd + (newStart - gapStart);

		if(newStart == gapStart)
		{
			
		}
		else if(newStart > gapStart)
		{
			System.arraycopy(text,gapEnd,text,gapStart,
				newStart - gapStart);
		}
		else if(newStart < gapStart)
		{
			System.arraycopy(text,newStart,text,newEnd,
				gapStart - newStart);
		}

		gapStart = newStart;
		gapEnd = newEnd;
	} 

	
	private void moveGapEnd(int newEnd)
	{
		System.arraycopy(text,gapEnd,text,newEnd,length - gapStart);
		gapEnd = newEnd;
	} 

	
	private void ensureCapacity(int capacity)
	{
		if(capacity >= text.length)
		{
			char[] textN = new char[capacity * 2];
			System.arraycopy(text,0,textN,0,length + (gapEnd - gapStart));
			text = textN;
		}
	} 

	
}
