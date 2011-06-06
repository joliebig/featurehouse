

package org.gjt.sp.util;

import java.awt.Color;
import java.awt.Font;
import java.util.Vector;

import org.gjt.sp.jedit.syntax.SyntaxStyle;



public class HtmlUtilities
{
	

	
	
	public static SyntaxStyle parseHighlightStyle(String style, Font f)
	{
		SyntaxStyle s;
		try
		{
			s = SyntaxUtilities.parseStyle(style, f.getFamily(), f.getSize(), true, null);
		}
		catch (Exception e)
		{
			style = "color:#000000";
			s = SyntaxUtilities.parseStyle(style, f.getFamily(), f.getSize(), true);
		}
		return s;
	} 

	
	
	public static String style2html(String prop, Font f)
	{
		StringBuilder tag = new StringBuilder();
		SyntaxStyle style = parseHighlightStyle(prop, f);
		Color c = style.getForegroundColor();
		if (c != null)
			tag.append("color:").append(color2html(c));
		c = style.getBackgroundColor();
		if (c != null)
			tag.append("background:").append(color2html(c));
		f = style.getFont();
		if (f.isBold())
			tag.append("font-weight:bold;");
		if (f.isItalic())
			tag.append("font-style: italic;");
		return tag.toString();
	} 

	
	
	public static String highlightString(String s, String styleTag, Vector<Integer> ranges)
	{
		StringBuilder sb = new StringBuilder("<html><style>.highlight {");
		sb.append(styleTag);
		sb.append("}</style><body>");
		int lastIndex = 0;
		for (int i = 0; i < ranges.size(); i += 2)
		{
			int rangeStart = ranges.get(i);
			int rangeEnd = ranges.get(i + 1);
			appendString2html(sb, s.substring(lastIndex, rangeStart));
			sb.append("<span class=\"highlight\">");
			appendString2html(sb, s.substring(rangeStart, rangeEnd));
			sb.append("</span>");
			lastIndex = rangeEnd;
		}
		appendString2html(sb, s.substring(lastIndex));
		sb.append("</body></html>");
		return sb.toString();
	} 

	
	
	public static void appendString2html(StringBuilder sb, String s)
	{
		for (int i = 0; i < s.length(); i++)
		{
			char c = s.charAt(i);
			String r;
			switch (c)
			{
			case '"':
				r = "&quot;";
				break;
			
			case '&':
				r = "&amp;";
				break;
			case '<':
				r = "&lt;";
				break;
			case '>':
				r = "&gt;";
				break;
			default:
				r = String.valueOf(c);
				break;
			}
			sb.append(r);
		}
	} 
	
	

	

	
	private static String color2html(Color c)
	{
		StringBuilder cs = new StringBuilder("rgb(");
		cs.append(c.getRed());
		cs.append(",");
		cs.append(c.getGreen());
		cs.append(",");
		cs.append(c.getBlue());
		cs.append(");");
		return cs.toString();
	} 

	
}
