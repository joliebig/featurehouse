

package installer;

import javax.swing.plaf.metal.*;
import javax.swing.plaf.*;
import java.awt.Font;

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

	
	private FontUIResource primaryFont = new FontUIResource("Dialog",
		Font.PLAIN,12);
	private FontUIResource secondaryFont = new FontUIResource("Dialog",
		Font.PLAIN,12);
}
