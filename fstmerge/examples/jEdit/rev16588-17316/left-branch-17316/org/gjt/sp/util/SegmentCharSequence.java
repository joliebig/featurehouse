
package org.gjt.sp.util;

import java.io.Serializable;
import javax.swing.text.Segment;


public class SegmentCharSequence implements CharSequence, Serializable
{

	public SegmentCharSequence(Segment seg)
	{
		this(seg, false);
	}

	
	@Deprecated
	public SegmentCharSequence(Segment seg, boolean reverse)
	{
		this(seg, 0, seg.count);
		this.reverse = reverse;
	}

	public SegmentCharSequence(Segment seg, int off, int len)
	{
		this.offset = off;
		this.length = len;
		this.seg = seg;
	}

	public char charAt(int index)
	{
		if (reverse)
			index = length - index - 1;
		return seg.array[seg.offset + offset + index];
	}

	public int length()
	{
		return length;
	}

	public CharSequence subSequence(int start, int end)
	{
		if (reverse)
			throw new IllegalStateException("reverse sub-sequences are not supported");
		return new SegmentCharSequence(seg, offset + start, end - start);
	}

	public String toString()
	{
		return new String(seg.array, offset+seg.offset, length);
	}

	private boolean reverse;
	private int 	offset;
	private int 	length;
	private Segment seg;

}

