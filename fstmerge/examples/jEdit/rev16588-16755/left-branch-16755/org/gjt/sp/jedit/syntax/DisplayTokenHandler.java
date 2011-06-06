

package org.gjt.sp.jedit.syntax;


import org.gjt.sp.jedit.buffer.JEditBuffer;

import javax.swing.text.*;
import java.awt.font.*;
import java.util.List;



public class DisplayTokenHandler extends DefaultTokenHandler
{
	
	public static final int MAX_CHUNK_LEN = 100;

	
	
	public void init(SyntaxStyle[] styles,
		FontRenderContext fontRenderContext,
		TabExpander expander, List<Chunk> out,
		float wrapMargin)
	{
		super.init();

		x = 0.0f;

		this.styles = styles;
		this.fontRenderContext = fontRenderContext;
		this.expander = expander;

		
		if(wrapMargin != 0.0f)
			this.wrapMargin = wrapMargin += 2.0f;
		else
			this.wrapMargin = 0.0f;

		this.out = out;

		seenNonWhitespace = false;
		endX = endOfWhitespace = 0.0f;
		end = null;
	} 

	
	
	public List<Chunk> getChunkList()
	{
		return out;
	} 

	
	
	public void handleToken(Segment seg, byte id, int offset, int length,
		TokenMarker.LineContext context)
	{
		if(id == Token.END)
		{
			if(firstToken != null)
				out.add(merge((Chunk)firstToken,seg));
			return;
		}

		for(int splitOffset = 0; splitOffset < length; splitOffset += MAX_CHUNK_LEN)
		{
			int splitLength = Math.min(length - splitOffset,MAX_CHUNK_LEN);
			Chunk chunk = createChunk(id,offset + splitOffset,splitLength,context);
			addToken(chunk,context);

			if(wrapMargin != 0.0f)
			{
				initChunk(chunk,seg);
				x += chunk.width;

				if(Character.isWhitespace(seg.array[
					seg.offset + chunk.offset]))
				{
					if(seenNonWhitespace)
					{
						end = lastToken;
						endX = x;
					}
					else
						endOfWhitespace = x;
				}
				else
				{
					if(x > wrapMargin
						&& end != null
						&& seenNonWhitespace)
					{
						Chunk nextLine = new Chunk(endOfWhitespace,
							end.offset + end.length,
							getParserRuleSet(context));
						initChunk(nextLine,seg);

						nextLine.next = end.next;
						end.next = null;

						if(firstToken != null)
							out.add(merge((Chunk)firstToken,seg));

						firstToken = nextLine;

						x = x - endX + endOfWhitespace;

						end = null;
						endX = x;
					}

					seenNonWhitespace = true;
				}
			}
		}
	} 

	

	
	private SyntaxStyle[] styles;
	private FontRenderContext fontRenderContext;
	private TabExpander expander;
	private float x;

	private List<Chunk> out;
	private float wrapMargin;
	private float endX;
	private Token end;

	private boolean seenNonWhitespace;
	private float endOfWhitespace;
	

	
	private Chunk createChunk(byte id, int offset, int length,
		TokenMarker.LineContext context)
	{
		return new Chunk(id,offset,length,
			getParserRuleSet(context),styles,
			context.rules.getDefault());
	} 

	
	protected void initChunk(Chunk chunk, Segment seg)
	{
		chunk.init(seg,expander,x,fontRenderContext);
	} 

	
	private Chunk merge(Chunk first, Segment seg)
	{
		if(first == null)
			return null;

		Chunk chunk = first;
		while(chunk.next != null)
		{
			Chunk next = (Chunk)chunk.next;
			if(canMerge(chunk,next,seg))
			{
				
				chunk.initialized = false;
				chunk.length += next.length;
				chunk.width += next.width;
				chunk.next = next.next;
			}
			else
			{
				if(!chunk.initialized)
				{
					initChunk(chunk,seg);
					if(wrapMargin == 0.0f)
						x += chunk.width;
				}
				chunk = next;
			}
		}

		if(!chunk.initialized)
			initChunk(chunk,seg);

		return first;
	} 

	
	private static boolean canMerge(Chunk c1, Chunk c2, Segment seg)
	{
		if(!c1.accessable || !c2.accessable)
			return false;

		char ch1 = seg.array[seg.offset + c1.offset];
		char ch2 = seg.array[seg.offset + c2.offset];

		return ((c1.style == c2.style)
			&& ch1 != '\t' && ch2 != '\t'
			&& (c1.length + c2.length <= MAX_CHUNK_LEN));
	} 

	
}
