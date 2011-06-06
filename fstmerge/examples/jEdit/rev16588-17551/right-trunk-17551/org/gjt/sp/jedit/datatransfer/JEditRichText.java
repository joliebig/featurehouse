
package org.gjt.sp.jedit.datatransfer;

import org.gjt.sp.jedit.Mode;
import org.gjt.sp.jedit.syntax.ModeProvider;


public class JEditRichText
{
	private final String text;

	private final String mode;

	public JEditRichText(String text, String mode)
	{
		this.text = text;
		this.mode = mode;
	}

	public String getText()
	{
		return text;
	}

	public Mode getMode()
	{
		return ModeProvider.instance.getMode(mode);
	}
}
