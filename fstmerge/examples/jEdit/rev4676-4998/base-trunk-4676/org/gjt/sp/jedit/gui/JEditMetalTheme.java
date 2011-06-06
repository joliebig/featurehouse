

package org.gjt.sp.jedit.gui;

import javax.swing.plaf.metal.*;
import javax.swing.plaf.*;
import org.gjt.sp.jedit.jEdit;

public class JEditMetalTheme extends DefaultMetalTheme
{
	public String getName()
	{
		return "jEdit";
	}

	public ColorUIResource getSystemTextColor()
	{
		return getBlack();
	}

	public FontUIResource getControlTextFont()
	{
		return primaryFont;
	}

	public FontUIResource getSystemTextFont()
	{
		return secondaryFont;
	}

	public FontUIResource getUserTextFont()
	{
		return secondaryFont;
	}

	public FontUIResource getMenuTextFont()
	{
		return primaryFont;
	}

	public void propertiesChanged()
	{
		primaryFont = new FontUIResource(
			jEdit.getFontProperty("metal.primary.font",
			super.getControlTextFont()));
		secondaryFont = new FontUIResource(
			jEdit.getFontProperty("metal.secondary.font",
			super.getSystemTextFont()));
	}

	
	private FontUIResource primaryFont;
	private FontUIResource secondaryFont;
}
