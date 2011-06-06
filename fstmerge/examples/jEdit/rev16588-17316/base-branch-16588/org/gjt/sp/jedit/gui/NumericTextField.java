
package org.gjt.sp.jedit.gui;

import javax.swing.*;
import java.awt.event.KeyEvent;


public class NumericTextField extends JTextField
{
	private final boolean positiveOnly;

	public NumericTextField(String text)
	{
		this(text, false);
	}

	public NumericTextField(String text, boolean positiveOnly)
	{
		super(text);
		this.positiveOnly = positiveOnly;
	}

	@Override
	protected void processKeyEvent(KeyEvent e)
	{
		if (e.getID() == KeyEvent.KEY_TYPED)
		{
			if (!Character.isDigit(e.getKeyChar()) && !(!positiveOnly && e.getKeyChar() == '-'))
			{
				e.consume();
			}
		}
		super.processKeyEvent(e);
	}
}
