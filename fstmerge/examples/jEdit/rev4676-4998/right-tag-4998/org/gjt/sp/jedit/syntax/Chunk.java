

package org.gjt.sp.jedit.syntax;


import javax.swing.text.*;
import java.awt.font.*;
import java.awt.geom.*;
import java.awt.*;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.Debug;



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

					if(glyphVector && chunks.gv != null)
						gfx.drawGlyphVector(chunks.gv,x + _x,y);
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

	
	public boolean accessable;
	public boolean visible;
	public boolean initialized;

	public boolean monospaced;
	public int charWidth;

	
	public SyntaxStyle style;
	
	
	public Color background;
	public float width;
	public GlyphVector gv;
	public String str;
	

	
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

	
	public final float[] getPositions()
	{
		if(gv == null)
			return null;

		if(positions == null)
			positions = gv.getGlyphPositions(0,length,null);

		return positions;
	} 

	
	public final float offsetToX(int offset)
	{
		if(!visible)
			return 0.0f;
		else if(monospaced)
			return offset * charWidth;
		else
			return getPositions()[offset * 2];
	} 

	
	public final int xToOffset(float x, boolean round)
	{
		if(!visible)
		{
			if(round && width - x < x)
				return offset + length;
			else
				return offset;
		}
		else if(monospaced)
		{
			x = Math.max(0,x);
			float remainder = x % charWidth;
			int i = (int)(x / charWidth);
			if(round && remainder > charWidth / 2)
				return offset + i + 1;
			else
				return offset + i;
		}
		else
		{
			float[] pos = getPositions();

			for(int i = 0; i < length; i++)
			{
				float glyphX = pos[i*2];
				float nextX = (i == length - 1
					? width : pos[i*2+2]);

				if(nextX > x)
				{
					if(!round || nextX - x > x - glyphX)
						return offset + i;
					else
						return offset + i + 1;
				}
			}
		}

		
		return -1;
	} 

	
	public void init(Segment seg, TabExpander expander, float x,
		FontRenderContext fontRenderContext)
	{
		initialized = true;
		if(style != null)
			charWidth = style.getCharWidth();

		if(!accessable)
		{
			
		}
		else if(length == 1 && seg.array[seg.offset + offset] == '\t')
		{
			visible = false;
			float newX = expander.nextTabStop(x,offset + length);
			width = newX - x;
		}
		else if(charWidth != 0 && !Debug.DISABLE_MONOSPACE_HACK)
		{
			visible = monospaced = true;
			str = new String(seg.array,seg.offset + offset,length);
			width = charWidth * length;
		}
		else
		{
			visible = true;
			str = new String(seg.array,seg.offset + offset,length);
			gv = style.getFont().createGlyphVector(
				fontRenderContext,str);
			width = (float)gv.getLogicalBounds().getWidth();
		}
	} 

	
	private float[] positions;
	
}
