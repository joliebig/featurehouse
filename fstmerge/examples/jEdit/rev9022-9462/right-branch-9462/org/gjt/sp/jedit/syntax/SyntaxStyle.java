
package org.gjt.sp.jedit.syntax;

import java.awt.Font;
import java.awt.Color;


public class SyntaxStyle
{
	
	
	public SyntaxStyle(Color fgColor, Color bgColor, Font font)
	{
		this.fgColor = fgColor;
		this.bgColor = bgColor;
		this.font = font;
	} 

	
	
	public Color getForegroundColor()
	{
		return fgColor;
	} 

	
	
	public Color getBackgroundColor()
	{
		return bgColor;
	} 

	
	
	public Font getFont()
	{
		return font;
	} 

	
	private Color fgColor;
	private Color bgColor;
	private Font font;
	
}
