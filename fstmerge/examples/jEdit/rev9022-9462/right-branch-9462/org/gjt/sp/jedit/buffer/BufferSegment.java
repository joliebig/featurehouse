

package org.gjt.sp.jedit.buffer;


public class BufferSegment implements CharSequence
{

	public BufferSegment(char[] data, int offset, int len)
	{
		this(data,offset,len,null);
	}

	private BufferSegment(char[] data, int offset, int len,
				BufferSegment next)
	{
		this.data = data;
		this.offset = offset;
		this.len = len;
		this.next = next;
	}

	public char charAt(int index)
	{
		if (index < len)
			return data[offset+index];
		else if (next != null)
			return next.charAt(index-len);
		else
			throw new ArrayIndexOutOfBoundsException(index);
	}

	public int length()
	{
		return len + ((next != null) ? next.length() : 0);
	}

	public CharSequence subSequence(int start, int end)
	{
		if (start >= 0 && end - start < len)
			return new BufferSegment(data,offset+start,end-start);
		else
			throw new ArrayIndexOutOfBoundsException();
	}

	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		toString(sb);
		if (next != null)
			next.toString(sb);
		return sb.toString();
	}

	protected BufferSegment concat(BufferSegment other)
	{
		BufferSegment clone = new BufferSegment(data,offset,len,next);
		BufferSegment last = clone;
		while (last.next != null)
			last = last.next;
		last.next = other;
		return clone;
	}

	private void toString(StringBuilder sb)
	{
		sb.append(data,offset,len);
	}

	private char[] data;
	private int offset;
	private int len;
	private BufferSegment next;
}

