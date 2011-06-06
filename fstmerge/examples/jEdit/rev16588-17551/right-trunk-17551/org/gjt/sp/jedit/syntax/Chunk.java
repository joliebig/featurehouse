

package org.gjt.sp.jedit.syntax;


import javax.swing.text.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.awt.*;
import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

import org.gjt.sp.jedit.Debug;
import org.gjt.sp.jedit.IPropertyManager;



public class Chunk extends Token
{
	
	
	public static float paintChunkList(Chunk chunks,
		Graphics2D gfx, float x, float y, boolean glyphVector)
	{
		Rectangle clipRect = gfx.getClipBounds();

		float _x = 0.0f;

		while(chunks != null)
		{
			
			if(x + _x + chunks.width > clipRect.x
				&& x + _x < clipRect.x + clipRect.width)
			{
				
				if(Debug.CHUNK_PAINT_DEBUG)
				{
					gfx.draw(new Rectangle2D.Float(x + _x,y - 10,
						chunks.width,10));
				}

				if(chunks.accessable && chunks.visible)
				{
					gfx.setFont(chunks.style.getFont());
					gfx.setColor(chunks.style.getForegroundColor());

					if (glyphVector && chunks.glyphs != null)
						chunks.drawGlyphs(gfx, x + _x, y);
					else if(chunks.str != null)
					{
						gfx.drawString(chunks.str,
							(int)(x + _x),(int)y);
					}
				}
			}

			_x += chunks.width;
			chunks = (Chunk)chunks.next;
		}

		return _x;
	} 

	
	
	public static float paintChunkBackgrounds(Chunk chunks,
		Graphics2D gfx, float x, float y)
	{
		Rectangle clipRect = gfx.getClipBounds();

		float _x = 0.0f;

		FontMetrics forBackground = gfx.getFontMetrics();

		int ascent = forBackground.getAscent();
		int height = forBackground.getHeight();

		while(chunks != null)
		{
			
			if(x + _x + chunks.width > clipRect.x
				&& x + _x < clipRect.x + clipRect.width)
			{
				if(chunks.accessable)
				{
					
					Color bgColor = chunks.background;
					if(bgColor != null)
					{
						gfx.setColor(bgColor);

						gfx.fill(new Rectangle2D.Float(
							x + _x,y - ascent,
							_x + chunks.width - _x,
							height));
					} 
				}
			}

			_x += chunks.width;
			chunks = (Chunk)chunks.next;
		}

		return _x;
	} 

	
	
	public static float offsetToX(Chunk chunks, int offset)
	{
		if(chunks != null && offset < chunks.offset)
		{
			throw new ArrayIndexOutOfBoundsException(offset + " < "
				+ chunks.offset);
		}

		float x = 0.0f;

		while(chunks != null)
		{
			if(chunks.accessable && offset < chunks.offset + chunks.length)
				return x + chunks.offsetToX(offset - chunks.offset);

			x += chunks.width;
			chunks = (Chunk)chunks.next;
		}

		return x;
	} 

	
	
	public static int xToOffset(Chunk chunks, float x, boolean round)
	{
		float _x = 0.0f;

		while(chunks != null)
		{
			if(chunks.accessable && x < _x + chunks.width)
				return chunks.xToOffset(x - _x,round);

			_x += chunks.width;
			chunks = (Chunk)chunks.next;
		}

		return -1;
	} 

	
	
	public static void propertiesChanged(IPropertyManager props)
	{
		fontSubstList = null;
		if (props == null)
		{
			fontSubstEnabled = false;
			preferredFonts = null;
		}


		int i = 0;
		String family;
		List<Font> userFonts = new ArrayList<Font>();

		fontSubstEnabled = Boolean.parseBoolean(props.getProperty("view.enableFontSubst"));

		while ((family = props.getProperty("view.fontSubstList." + i)) != null)
		{
			
			Font f = new Font(family, Font.PLAIN, 12);
			if (!f.getFamily().equalsIgnoreCase("dialog") ||
			    family.equalsIgnoreCase("dialog"))
				userFonts.add(f);
			i++;
		}

		preferredFonts = userFonts.toArray(new Font[userFonts.size()]);
	} 

	
	public boolean accessable;
	public boolean initialized;

	
	public SyntaxStyle style;
	public float width;
	

	
	public Chunk(float width, int offset, ParserRuleSet rules)
	{
		super(Token.NULL,offset,0,rules);
		this.width = width;
	} 

	
	public Chunk(byte id, int offset, int length, ParserRuleSet rules,
		SyntaxStyle[] styles, byte defaultID)
	{
		super(id,offset,length,rules);
		accessable = true;
		style = styles[id];
		background = style.getBackgroundColor();
		if(background == null)
			background = styles[defaultID].getBackgroundColor();
	} 

	
	public final float offsetToX(int offset)
	{
		if(!visible || glyphs == null)
			return 0.0f;

		float x = 0.0f;
		for (GlyphVector gv : glyphs)
		{
			if (offset < gv.getNumGlyphs())
			{
				x += (float) gv.getGlyphPosition(offset).getX();
				return x;
			}
			x += (float) gv.getLogicalBounds().getWidth();
			offset -= gv.getNumGlyphs();
		}

		
		assert false : "Shouldn't reach this.";
		return -1;
	} 

	
	public final int xToOffset(float x, boolean round)
	{
		if (!visible || glyphs == null)
		{
			if (round && width - x < x)
				return offset + length;
			else
				return offset;
		}

		int off = offset;
		float myx = 0.0f;
		for (GlyphVector gv : glyphs)
		{
			float gwidth = (float) gv.getLogicalBounds().getWidth();
			if (myx + gwidth >= x)
			{
				float[] pos = gv.getGlyphPositions(0, gv.getNumGlyphs(), null);
				for (int i = 0; i < gv.getNumGlyphs(); i++)
				{
					float glyphX = myx + pos[i * 2];
					float nextX = (i == gv.getNumGlyphs() - 1)
					            ? width
					            : myx + pos[i * 2 + 2];

					if (nextX > x)
					{
						if (!round || nextX - x > x - glyphX)
							return off + i;
						else
							return off + i + 1;
					}
				}
			}
			myx += gwidth;
			off += gv.getNumGlyphs();
		}

		
		assert false : "Shouldn't reach this.";
		return -1;
	} 

	
	public void init(Segment seg, TabExpander expander, float x,
		FontRenderContext fontRenderContext)
	{
		initialized = true;

		if(!accessable)
		{
			
		}
		else if(length == 1 && seg.array[seg.offset + offset] == '\t')
		{
			visible = false;
			float newX = expander.nextTabStop(x,offset + length);
			width = newX - x;
		}
		else
		{
			visible = true;

			str = new String(seg.array,seg.offset + offset,length);

			char[] textArray = seg.array;
			int textStart = seg.offset + offset;
			
			
			if (SUN_JAVA_5)
			{
				
				
				
				char[] copy = new char[length];
				System.arraycopy(textArray, textStart,
					copy, 0, length);
				textArray = copy;
				textStart = 0;
			} 
			width = layoutGlyphs(fontRenderContext,
					     textArray,
					     textStart,
					     textStart + length);
		}
	} 

	
	
	
	private Color background;
	private String str;
	
	private List<GlyphVector> glyphs;
	private boolean visible;

	private static boolean fontSubstEnabled;
	private static Font[] preferredFonts;
	private static Font[] fontSubstList;

	
	private static final boolean SUN_JAVA_5;
	static
	{
		boolean sun_java_5 = false;
		String vendor = System.getProperty("java.vendor");
		
		
		if (vendor != null &&
			(vendor.startsWith("Sun") ||
			vendor.startsWith("Apple") ||
			vendor.startsWith("IBM")))
		{
			String version = System.getProperty("java.version");
			if (version != null && version.startsWith("1.5"))
			{
				sun_java_5 = true;
			}
		}
		SUN_JAVA_5 = sun_java_5;
	}

	
	
	private static Font[] getFonts()
	{
		if (fontSubstList == null)
		{
			Font[] systemFonts = GraphicsEnvironment.getLocalGraphicsEnvironment().getAllFonts();

			fontSubstList = new Font[preferredFonts.length +
						 systemFonts.length];

			System.arraycopy(preferredFonts, 0, fontSubstList, 0,
					 preferredFonts.length);

			System.arraycopy(systemFonts, 0, fontSubstList,
					 preferredFonts.length,
					 systemFonts.length);
		}
		return fontSubstList;
	} 

	
	
	private void drawGlyphs(Graphics2D gfx,
				float x,
				float y)
	{
		for (GlyphVector gv : glyphs)
		{
			gfx.drawGlyphVector(gv, x, y);
			x += (float) gv.getLogicalBounds().getWidth();
		}
	} 

	
	
	private float addGlyphVector(Font f,
				     FontRenderContext frc,
				     char[] text,
				     int start,
				     int end)
	{
		
		int layoutFlags = Font.LAYOUT_LEFT_TO_RIGHT
			| Font.LAYOUT_NO_START_CONTEXT
			| Font.LAYOUT_NO_LIMIT_CONTEXT;

		GlyphVector gv = f.layoutGlyphVector(frc,
						     text,
						     start,
						     end,
						     layoutFlags);
		glyphs.add(gv);
		return (float) gv.getLogicalBounds().getWidth();
	} 

	
	
	private float layoutGlyphs(FontRenderContext frc,
				   char text[],
				   int start,
				   int end)
	{
		float width = 0.0f;
		int max = 0;
		Font dflt = style.getFont();

		glyphs = new LinkedList<GlyphVector>();

		while (max != -1 && start < end)
		{
			max = fontSubstEnabled ? dflt.canDisplayUpTo(text, start, end)
			                       : -1;
			if (max == -1)
			{
				width += addGlyphVector(dflt,
							frc,
							text,
							start,
							end);
			}
			else
			{
				
				if (max > start)
				{
					width += addGlyphVector(dflt,
								frc,
								text,
								start,
								max);
					start = max;
				}

				
				Font f = null;
				for (Font candidate : getFonts())
				{
					 if (candidate.canDisplay(text[start]))
					 {
						 f = candidate;
						 break;
					 }
				}

				if (f != null)
				{
					f = f.deriveFont(dflt.getStyle(), dflt.getSize());

					
					int last = start;
					while (last < end &&
					       f.canDisplay(text[last]) &&
					       !dflt.canDisplay(text[last]))
						last++;

					width += addGlyphVector(f,
								frc,
								text,
								start,
								last);

					start = last;
				}
				else
				{
					width += addGlyphVector(dflt,
								frc,
								text,
								start,
								start + 1);
					start++;
				}
			}
		}
		return width;
	} 

	
}
