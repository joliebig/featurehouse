

package org.gjt.sp.jedit.buffer;


class BufferSegment implements CharSequence
{

	public BufferSegment(char[] data,
			     int offset,
			     int len)
	{
		this(data,offset,len,null);
	}

	public BufferSegment(char[] data,
			      int offset,
			      int len,
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

	public CharSequence subSequence(int start,
					int end)
	{
		return subSegment(start, end);
	}

	public String toString()
	{
		StringBuilder sb = new StringBuilder();
		toString(sb);
		return sb.toString();
	}

	private void toString(StringBuilder sb)
	{
		sb.append(data,offset,len);
		if (next != null)
			next.toString(sb);
	}

	private BufferSegment subSegment(int start,
					int end)
	{
		if (0 <= start && start <= end)
			if (end <= len)
				return new BufferSegment(data,offset+start,
					end-start);
			else if (next != null)
				if (start < len)
					return new BufferSegment(data,
						offset+start,len-start,
						next.subSegment(0,end-len));
				else
					return next.subSegment(start-len,
						end-len);
			else
				throw new ArrayIndexOutOfBoundsException();
		else
			throw new ArrayIndexOutOfBoundsException();
	}

	private final char[] data;
	private final int offset;
	private final int len;
	private final BufferSegment next;
}

