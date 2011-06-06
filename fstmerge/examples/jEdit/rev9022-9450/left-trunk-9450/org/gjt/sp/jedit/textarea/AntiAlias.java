
package org.gjt.sp.jedit.textarea;


public class AntiAlias extends Object
{
	public static final Object NONE = "none";

	public static final Object STANDARD = "standard";

	public static final Object SUBPIXEL = "subpixel";

	public static final Object comboChoices[] = new Object[] { NONE, STANDARD, SUBPIXEL };

	public void set(int newValue)
	{
		m_val = newValue;
	}

	public AntiAlias(boolean isEnabled)
	{
		m_val = isEnabled ? 1 : 0;
	}

	public AntiAlias(int val)
	{
		m_val = val;
	}

	public AntiAlias(String v)
	{
		fromString(v);
	}

	public boolean equals(Object other)
	{
		return toString().equals(other.toString());

	}

	public void fromString(String v)
	{
		for (int i = 0; i < comboChoices.length; ++i)
		{
			if (comboChoices[i].equals(v))
			{
				m_val = i;
			}
		}
	}

	public String toString()
	{
		return comboChoices[m_val].toString();
	}

	public int val()
	{
		return m_val;
	}

	private int m_val = 0;
}
