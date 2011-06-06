

package org.gjt.sp.util;


public class ReverseCharSequence implements CharSequence
{
	public ReverseCharSequence(CharSequence base)
	{
		this.base = base;
	}

	public CharSequence baseSequence()
	{
		return base;
	}

	public char charAt(int index)
	{
		return base.charAt(base.length() - index - 1);
	}

	public int length()
	{
		return base.length();
	}

	public CharSequence subSequence(int start, int end)
	{
		int baseLength = base.length();
		return new ReverseCharSequence(
			base.subSequence(baseLength - end, baseLength - start));
	}

	public String toString()
	{
		int baseLength = base.length();
		StringBuilder builder = new StringBuilder(baseLength);
		for (int i = baseLength - 1; i >= 0; --i)
		{
			builder.append(base.charAt(i));
		}
		return builder.toString();
	}

	private final CharSequence base;
}
