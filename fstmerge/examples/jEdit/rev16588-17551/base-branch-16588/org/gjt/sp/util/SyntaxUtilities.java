

package org.gjt.sp.util;



import java.awt.Color;
import java.awt.Font;
import java.util.Locale;
import java.util.StringTokenizer;
import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.syntax.Token;
import org.gjt.sp.jedit.IPropertyManager;



public class SyntaxUtilities
{
	public static IPropertyManager propertyManager;
	
	
	
	
	public static String getColorHexString(Color c)
	{
		String colString = Integer.toHexString(c.getRGB() & 0xffffff);
		return "#000000".substring(0,7 - colString.length()).concat(colString);
	} 
	
	
	
	public static Color parseColor(String name, Color defaultColor)
	{
		if(name == null || name.length() == 0)
			return defaultColor;
		else if(name.charAt(0) == '#')
		{
			try
			{
				return Color.decode(name);
			}
			catch(NumberFormatException nf)
			{
				return defaultColor;
			}
		}
		else if("red".equals(name))
			return Color.red;
		else if("green".equals(name))
			return Color.green;
		else if("blue".equals(name))
			return Color.blue;
		else if("yellow".equals(name))
			return Color.yellow;
		else if("orange".equals(name))
			return Color.orange;
		else if("white".equals(name))
			return Color.white;
		else if("lightGray".equals(name))
			return Color.lightGray;
		else if("gray".equals(name))
			return Color.gray;
		else if("darkGray".equals(name))
			return Color.darkGray;
		else if("black".equals(name))
			return Color.black;
		else if("cyan".equals(name))
			return Color.cyan;
		else if("magenta".equals(name))
			return Color.magenta;
		else if("pink".equals(name))
			return Color.pink;
		else
			return defaultColor;
	} 
	
	
	
	public static SyntaxStyle parseStyle(String str, String family, int size,
		boolean color, Color defaultFgColor)
		throws IllegalArgumentException
	{
		Color fgColor = defaultFgColor;
		Color bgColor = null;
		boolean italic = false;
		boolean bold = false;
		StringTokenizer st = new StringTokenizer(str);
		while(st.hasMoreTokens())
		{
			String s = st.nextToken();
			if(s.startsWith("color:"))
			{
				if(color)
					fgColor = parseColor(s.substring(6), Color.black);
			}
			else if(s.startsWith("bgColor:"))
			{
				if(color)
					bgColor = parseColor(s.substring(8), null);
			}
			else if(s.startsWith("style:"))
			{
				for(int i = 6; i < s.length(); i++)
				{
					if(s.charAt(i) == 'i')
						italic = true;
					else if(s.charAt(i) == 'b')
						bold = true;
					else
						throw new IllegalArgumentException(
								"Invalid style: " + s);
				}
			}
			else
				throw new IllegalArgumentException(
						"Invalid directive: " + s);
		}
		return new SyntaxStyle(fgColor,bgColor,
				new Font(family,
						(italic ? Font.ITALIC : 0) | (bold ? Font.BOLD : 0),
						size));
	} 
		
	
	
	public static SyntaxStyle parseStyle(String str, String family, int size,
		boolean color)
		throws IllegalArgumentException
	{
		return parseStyle(str, family, size, color, Color.black);
	} 
	
	
	
	public static SyntaxStyle[] loadStyles(String family, int size)
	{
		return loadStyles(family,size,true);
	}
	
	
	public static SyntaxStyle[] loadStyles(String family, int size, boolean color)
	{
		SyntaxStyle[] styles = new SyntaxStyle[Token.ID_COUNT];

		
		for(int i = 1; i < styles.length; i++)
		{
			try
			{
				String styleName = "view.style."
					+ Token.tokenToString((byte)i)
					.toLowerCase(Locale.ENGLISH);
				styles[i] = parseStyle(
					propertyManager.getProperty(styleName),
					family,size,color);
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,StandardUtilities.class,e);
			}
		}

		return styles;
	} 
	
	private SyntaxUtilities(){}
}
