

package org.gjt.sp.jedit.textarea;


import javax.swing.text.*;
import javax.swing.JComponent;

import java.awt.event.MouseEvent;
import java.awt.font.*;
import java.awt.geom.AffineTransform;
import java.awt.*;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.util.*;
import org.gjt.sp.jedit.buffer.IndentFoldHandler;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.syntax.Chunk;
import org.gjt.sp.jedit.syntax.DefaultTokenHandler;
import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.syntax.Token;
import org.gjt.sp.jedit.Debug;

import org.gjt.sp.util.Log;



public class TextAreaPainter extends JComponent implements TabExpander
{
	
	
	public static final int LOWEST_LAYER = Integer.MIN_VALUE;

	
	public static final int BACKGROUND_LAYER = -60;

	
	public static final int LINE_BACKGROUND_LAYER = -50;

	
	public static final int BELOW_SELECTION_LAYER = -40;

	
	public static final int SELECTION_LAYER = -30;

	
	public static final int WRAP_GUIDE_LAYER = -20;

	
	public static final int BELOW_MOST_EXTENSIONS_LAYER = -10;

	
	public static final int DEFAULT_LAYER = 0;

	
	public static final int BLOCK_CARET_LAYER = 50;

	
	public static final int BRACKET_HIGHLIGHT_LAYER = 100;

	
	public static final int TEXT_LAYER = 200;

	
	public static final int CARET_LAYER = 300;

	
	public static final int HIGHEST_LAYER = Integer.MAX_VALUE;
	

	
	
	@Override
	public void setBounds(int x, int y, int width, int height)
	{
		if(x == getX() && y == getY() && width == getWidth()
			&& height == getHeight())
		{
			return;
		}

		super.setBounds(x,y,width,height);

		textArea.recalculateVisibleLines();
		if(!textArea.getBuffer().isLoading())
			textArea.recalculateLastPhysicalLine();
		textArea.propertiesChanged();
		textArea.updateMaxHorizontalScrollWidth();
		textArea.scrollBarsInitialized = true;
	} 

	
	
	@Override
	public boolean getFocusTraversalKeysEnabled()
	{
		return false;
	} 

	

	
	
	public final SyntaxStyle[] getStyles()
	{
		return styles;
	} 

	
	
	public final void setStyles(SyntaxStyle[] styles)
	{
		
		
		
		fonts.clear();

		this.styles = styles;
		styles[Token.NULL] = new SyntaxStyle(getForeground(),null,getFont());
		repaint();
	} 

	
	
	public final Color getCaretColor()
	{
		return caretColor;
	} 

	
	
	public final void setCaretColor(Color caretColor)
	{
		this.caretColor = caretColor;
		if(textArea.getBuffer() != null)
			textArea.invalidateLine(textArea.getCaretLine());
	} 

	
	
	public final Color getSelectionColor()
	{
		return selectionColor;
	} 

	
	
	public final void setSelectionColor(Color selectionColor)
	{
		this.selectionColor = selectionColor;
		textArea.repaint();
	} 

	
	
	public final Color getMultipleSelectionColor()
	{
		return multipleSelectionColor;
	} 

	
	
	public final void setMultipleSelectionColor(Color multipleSelectionColor)
	{
		this.multipleSelectionColor = multipleSelectionColor;
		textArea.repaint();
	} 

	
	
	public final Color getLineHighlightColor()
	{
		return lineHighlightColor;
	} 

	
	
	public final void setLineHighlightColor(Color lineHighlightColor)
	{
		this.lineHighlightColor = lineHighlightColor;
		if(textArea.getBuffer() != null)
			textArea.invalidateLine(textArea.getCaretLine());
	} 

	
	
	public final boolean isLineHighlightEnabled()
	{
		return lineHighlight;
	} 

	
	
	public final void setLineHighlightEnabled(boolean lineHighlight)
	{
		this.lineHighlight = lineHighlight;
		textArea.repaint();
	} 

	
	
	public final Color getSelectionFgColor()
	{
		return selectionFgColor;
	} 

	
	
	public final void setSelectionFgColor(Color selectionFgColor)
	{
		this.selectionFgColor = selectionFgColor;
		if (isSelectionFgColorEnabled())
			textArea.repaint();
	} 

	
	
	public final boolean isSelectionFgColorEnabled()
	{
		return selectionFg;
	} 

	
	
	public final void setSelectionFgColorEnabled(boolean selectionFg)
	{
		this.selectionFg = selectionFg;
		textArea.repaint();
	} 

	
	
	public final Color getStructureHighlightColor()
	{
		return structureHighlightColor;
	} 

	
	
	public final void setStructureHighlightColor(
		Color structureHighlightColor)
	{
		this.structureHighlightColor = structureHighlightColor;
		textArea.invalidateStructureMatch();
	} 

	
	
	public final boolean isStructureHighlightEnabled()
	{
		return structureHighlight;
	} 

	
	
	public final void setStructureHighlightEnabled(boolean structureHighlight)
	{
		this.structureHighlight = structureHighlight;
		textArea.invalidateStructureMatch();
	} 

	
	
	public final boolean isBlockCaretEnabled()
	{
		return blockCaret;
	} 

	
	
	public final void setBlockCaretEnabled(boolean blockCaret)
	{
		this.blockCaret = blockCaret;
		extensionMgr.removeExtension(caretExtension);
		if(blockCaret)
			addExtension(BLOCK_CARET_LAYER,caretExtension);
		else
			addExtension(CARET_LAYER,caretExtension);
		if(textArea.getBuffer() != null)
			textArea.invalidateLine(textArea.getCaretLine());
	} 

	
	
	public final boolean isThickCaretEnabled()
	{
		return thickCaret;
	} 

	
	
	public final void setThickCaretEnabled(boolean thickCaret)
	{
		this.thickCaret = thickCaret;
		if(textArea.getBuffer() != null)
			textArea.invalidateLine(textArea.getCaretLine());
	} 

	
	
	public final Color getEOLMarkerColor()
	{
		return eolMarkerColor;
	} 

	
	
	public final void setEOLMarkerColor(Color eolMarkerColor)
	{
		this.eolMarkerColor = eolMarkerColor;
		repaint();
	} 

	
	
	public final boolean getEOLMarkersPainted()
	{
		return eolMarkers;
	} 

	
	
	public final void setEOLMarkersPainted(boolean eolMarkers)
	{
		this.eolMarkers = eolMarkers;
		repaint();
	} 

	
	
	public final Color getWrapGuideColor()
	{
		return wrapGuideColor;
	} 

	
	
	public final void setWrapGuideColor(Color wrapGuideColor)
	{
		this.wrapGuideColor = wrapGuideColor;
		repaint();
	} 

	
	
	public final boolean isWrapGuidePainted()
	{
		return wrapGuide;
	} 

	
	
	public final void setWrapGuidePainted(boolean wrapGuide)
	{
		this.wrapGuide = wrapGuide;
		repaint();
	} 

	
	
	public final SyntaxStyle[] getFoldLineStyle()
	{
		return foldLineStyle;
	} 

	
	
	public final void setFoldLineStyle(SyntaxStyle[] foldLineStyle)
	{
		this.foldLineStyle = foldLineStyle;
		repaint();
	} 

	
	
	@Deprecated
	public void setAntiAliasEnabled(boolean isEnabled)
	{
		setAntiAlias(new AntiAlias(isEnabled));
	}
	
	public void setAntiAlias(AntiAlias newValue)
	{
		antiAlias = newValue;
		updateRenderingHints();
	} 

	
	public AntiAlias getAntiAlias()
	{
		return antiAlias;
	}

	
	
	@Deprecated
	public boolean isAntiAliasEnabled()
	{
		return antiAlias.val() > 0;
	} 

	
	
	public void setFractionalFontMetricsEnabled(boolean fracFontMetrics)
	{
		this.fracFontMetrics = fracFontMetrics;
		updateRenderingHints();
	} 

	
	
	public boolean isFractionalFontMetricsEnabled()
	{
		return fracFontMetrics;
	} 

	
	
	public FontRenderContext getFontRenderContext()
	{
		return fontRenderContext;
	} 

	

	
	
	public void addExtension(TextAreaExtension extension)
	{
		extensionMgr.addExtension(DEFAULT_LAYER,extension);
		repaint();
	} 

	
	
	public void addExtension(int layer, TextAreaExtension extension)
	{
		extensionMgr.addExtension(layer,extension);
		repaint();
	} 

	
	
	public void removeExtension(TextAreaExtension extension)
	{
		extensionMgr.removeExtension(extension);
		repaint();
	} 

	
	
	public TextAreaExtension[] getExtensions()
	{
		return extensionMgr.getExtensions();
	} 

	
	
	@Override
	public String getToolTipText(MouseEvent evt)
	{
		if(textArea.getBuffer().isLoading())
			return null;

		return extensionMgr.getToolTipText(evt.getX(),evt.getY());
	} 

	
	
	public FontMetrics getFontMetrics()
	{
		return fm;
	} 

	
	
	@Override
	public void setFont(Font font)
	{
		super.setFont(font);
		fm = getFontMetrics(font);
		textArea.recalculateVisibleLines();
		if(textArea.getBuffer() != null
			&& !textArea.getBuffer().isLoading())
			textArea.recalculateLastPhysicalLine();
		
	} 

	
	
	public float getStringWidth(String str)
	{
		if(textArea.charWidth != 0)
			return textArea.charWidth * str.length();
		else
		{
			return (float)getFont().getStringBounds(
				str,getFontRenderContext()).getWidth();
		}
	} 

	
	
	@Override
	public void update(Graphics _gfx)
	{
		paint(_gfx);
	} 

	
	
	@Override
	public void paint(Graphics _gfx)
	{
		assert _gfx instanceof Graphics2D;
		Graphics2D gfx = (Graphics2D)_gfx;
		gfx.setRenderingHints(renderingHints);
		fontRenderContext = gfx.getFontRenderContext();

		Rectangle clipRect = gfx.getClipBounds();
		int lineHeight = fm.getHeight();
		if(lineHeight == 0 || textArea.getBuffer().isLoading())
		{
			gfx.setColor(getBackground());
			gfx.fillRect(clipRect.x,clipRect.y,clipRect.width,clipRect.height);
		}
		else
		{
			long prepareTime = System.nanoTime();
			
			
			
			int firstLine = clipRect.y / lineHeight;
			int lastLine = (clipRect.y + clipRect.height - 1) / lineHeight;
			gfx.setColor(getBackground());
			gfx.setFont(getFont());
			prepareTime = System.nanoTime() - prepareTime;

			long linesTime = System.nanoTime();
			int numLines = lastLine - firstLine + 1;
			int y = firstLine * lineHeight;
			gfx.fillRect(0,y,getWidth(),numLines * lineHeight);
			extensionMgr.paintScreenLineRange(textArea,gfx,
				firstLine,lastLine,y,lineHeight);
			linesTime = System.nanoTime() - linesTime;

			if(Debug.PAINT_TIMER && numLines >= 1)
				Log.log(Log.DEBUG,this,"repainting " + numLines + " lines took " + prepareTime + "/" + linesTime + " ns");
		}

		textArea.updateMaxHorizontalScrollWidth();
	} 

	
	
	public float nextTabStop(float x, int tabOffset)
	{
		int ntabs = (int)(x / textArea.tabSize);
		return (ntabs + 1) * textArea.tabSize;
	} 

	
	
	@Override
	public Dimension getPreferredSize()
	{
		Dimension dim = new Dimension();

		char[] foo = new char[80];
		for(int i = 0; i < foo.length; i++)
			foo[i] = ' ';
		dim.width = (int)getStringWidth(new String(foo));
		dim.height = fm.getHeight() * 25;
		return dim;
	} 

	
	
	@Override
	public Dimension getMinimumSize()
	{
		return getPreferredSize();
	} 

	

	
	
	TextArea textArea;

	SyntaxStyle[] styles;
	Color caretColor;
	Color selectionColor;
	Color multipleSelectionColor;
	Color lineHighlightColor;
	Color structureHighlightColor;
	Color eolMarkerColor;
	Color wrapGuideColor;

	SyntaxStyle[] foldLineStyle;

	boolean blockCaret;
	boolean thickCaret;
	boolean lineHighlight;
	boolean structureHighlight;
	boolean eolMarkers;
	boolean wrapGuide;
	AntiAlias antiAlias;
	boolean fracFontMetrics;
	RenderingHints renderingHints;
	boolean selectionFg;
	Color selectionFgColor;
	
	FontMetrics fm;
	

	
	
	TextAreaPainter(TextArea textArea)
	{
		enableEvents(AWTEvent.FOCUS_EVENT_MASK
			| AWTEvent.KEY_EVENT_MASK
			| AWTEvent.MOUSE_EVENT_MASK);

		this.textArea = textArea;
		antiAlias = new AntiAlias(0);
		fonts = new HashMap();
		extensionMgr = new ExtensionManager();

		setAutoscrolls(true);
		setOpaque(true);
		setRequestFocusEnabled(false);
		setDoubleBuffered(false);

		setCursor(Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR));

		fontRenderContext = new FontRenderContext(null,false,false);

		addExtension(LINE_BACKGROUND_LAYER,new PaintLineBackground());
		addExtension(SELECTION_LAYER,new PaintSelection());
		addExtension(WRAP_GUIDE_LAYER,new PaintWrapGuide());
		addExtension(BRACKET_HIGHLIGHT_LAYER,new StructureMatcher
			.Highlight(textArea));
		addExtension(TEXT_LAYER,new PaintText());
		addExtension(TEXT_LAYER,new PaintSelectionText());
		caretExtension = new PaintCaret();
	} 

	

	

	
	private final ExtensionManager extensionMgr;
	private final PaintCaret caretExtension;
	private FontRenderContext fontRenderContext;
	private final Map fonts;
	

	private static Object sm_hrgbRender;
	private static Constructor<FontRenderContext> sm_frcConstructor;

	static
	{
		try
		{
			Field f = RenderingHints.class.getField("VALUE_TEXT_ANTIALIAS_LCD_HRGB");
			sm_hrgbRender = f.get(null);
			Class[] fracFontMetricsTypeList = {AffineTransform.class, Object.class, Object.class};
			sm_frcConstructor = FontRenderContext.class.getConstructor(fracFontMetricsTypeList);
		}
		catch (NullPointerException npe) {}
		catch (SecurityException se) {}
		catch (NoSuchFieldException nsfe) {}
		catch (IllegalArgumentException iae) {}
		catch (IllegalAccessException iae) {}
		catch (NoSuchMethodException nsme) {}
	}
	
	private void updateRenderingHints()
	{
		Map<RenderingHints.Key,Object> hints = new HashMap<RenderingHints.Key,Object>();

		hints.put(RenderingHints.KEY_FRACTIONALMETRICS,
			fracFontMetrics ? RenderingHints.VALUE_FRACTIONALMETRICS_ON
				: RenderingHints.VALUE_FRACTIONALMETRICS_OFF);

		if (antiAlias.val() == 0)
		{
			hints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
			hints.put(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
			fontRenderContext = new FontRenderContext(null, antiAlias.val() > 0, fracFontMetrics);
		}
		
		else if (antiAlias.val() == 2 && sm_hrgbRender != null )
		{
			hints.put(RenderingHints.KEY_TEXT_ANTIALIASING, sm_hrgbRender);
			Object fontRenderHint = fracFontMetrics ?
				RenderingHints.VALUE_FRACTIONALMETRICS_ON :
				RenderingHints.VALUE_FRACTIONALMETRICS_OFF;
			Object[] paramList = {null, sm_hrgbRender, fontRenderHint};
			try
			{
				fontRenderContext = sm_frcConstructor.newInstance(paramList);
			}
			catch (Exception e)
			{
				fontRenderContext = new FontRenderContext(null, antiAlias.val() > 0, fracFontMetrics);
			}
		}
		else 
		{
			hints.put(RenderingHints.KEY_RENDERING, RenderingHints.VALUE_RENDER_QUALITY);
			hints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			hints.put(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
			fontRenderContext = new FontRenderContext(null, antiAlias.val() > 0, fracFontMetrics);
		}

		renderingHints = new RenderingHints(hints);


	} 

	

	

	
	private class PaintLineBackground extends TextAreaExtension
	{
		
		private boolean shouldPaintLineHighlight(int caret, int start, int end)
		{
			if(!isLineHighlightEnabled()
				|| caret < start || caret >= end)
			{
				return false;
			}

			int count = textArea.getSelectionCount();
			if(count == 1)
			{
				Selection s = textArea.getSelection(0);
				return s.getStartLine() == s.getEndLine();
			}
			else
				return count == 0;
		} 

		
		@Override
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			
			TextArea textArea = TextAreaPainter.this.textArea;
			JEditBuffer buffer = textArea.getBuffer();

			
			boolean collapsedFold =
				physicalLine < buffer.getLineCount() - 1
				&& buffer.isFoldStart(physicalLine)
				&& !textArea.displayManager
				.isLineVisible(physicalLine + 1);

			SyntaxStyle foldLineStyle = null;
			if(collapsedFold)
			{
				int level = buffer.getFoldLevel(physicalLine + 1);
				if(buffer.getFoldHandler() instanceof IndentFoldHandler)
					level = Math.max(1,level / buffer.getIndentSize());
				if(level > 3)
					level = 0;
				foldLineStyle = TextAreaPainter.this.foldLineStyle[level];
			}

			int caret = textArea.getCaretPosition();
			boolean paintLineHighlight = shouldPaintLineHighlight(
				caret,start,end);

			Color bgColor;
			if(paintLineHighlight)
				bgColor = lineHighlightColor;
			else if(collapsedFold)
			{
				bgColor = foldLineStyle.getBackgroundColor();
				if(bgColor == null)
					bgColor = getBackground();
			}
			else
				bgColor = getBackground();

			if(paintLineHighlight || collapsedFold)
			{
				gfx.setColor(bgColor);
				gfx.fillRect(0,y,getWidth(),fm.getHeight());
			} 

			
			ChunkCache.LineInfo lineInfo = textArea.chunkCache
				.getLineInfo(screenLine);

			if(lineInfo.chunks != null)
			{
				float baseLine = y + fm.getHeight()
					- (fm.getLeading()+1) - fm.getDescent();
				Chunk.paintChunkBackgrounds(
					lineInfo.chunks,gfx,
					textArea.getHorizontalOffset(),
					baseLine);
			} 
		} 
	} 

	
	private class PaintSelection extends TextAreaExtension
	{
		
		@Override
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			if(textArea.getSelectionCount() == 0)
				return;

			gfx.setColor(textArea.isMultipleSelectionEnabled()
				? getMultipleSelectionColor()
				: getSelectionColor());

			Iterator<Selection> iter = textArea.getSelectionIterator();
			while(iter.hasNext())
			{
				Selection s = iter.next();
				paintSelection(gfx,screenLine,physicalLine,y,s);
			}
		} 

		
		private void paintSelection(Graphics2D gfx, int screenLine,
			int physicalLine, int y, Selection s)
		{
			int[] selectionStartAndEnd
				= textArea.selectionManager
				.getSelectionStartAndEnd(
				screenLine,physicalLine,s);
			if(selectionStartAndEnd == null)
				return;

			int x1 = selectionStartAndEnd[0];
			int x2 = selectionStartAndEnd[1];

			gfx.fillRect(x1,y,x2 - x1,fm.getHeight());
		} 
	} 

	
	private class PaintSelectionText extends TextAreaExtension
	{
		
		@Override
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			if(textArea.getSelectionCount() == 0)
				return;
			if ((! isSelectionFgColorEnabled()) || (getSelectionFgColor() == null)) 
				return;

			Iterator<Selection> iter = textArea.getSelectionIterator();
			while(iter.hasNext())
			{
				Selection s = iter.next();
				paintSelection(gfx,screenLine,physicalLine,y,s);
			}
		} 

		
		private void paintSelection(Graphics2D gfx, int screenLine,
			int physicalLine, int y, Selection s)
		{
			if ((physicalLine < s.getStartLine()) || (physicalLine > s.getEndLine()))
				return;
			float x = 0f;
			float baseLine = y + fm.getHeight() -
				(fm.getLeading()+1) - fm.getDescent();

			DefaultTokenHandler tokenHandler = new DefaultTokenHandler();
			textArea.getBuffer().markTokens(physicalLine, tokenHandler);
			Token token = tokenHandler.getTokens();

			int lineStart = textArea.getLineStartOffset(physicalLine);
			int startOffset, endOffset;
			if (s instanceof Selection.Rect)
			{
				Selection.Rect r = (Selection.Rect)s;
				startOffset = r.getStart(textArea.getBuffer(), physicalLine);
				endOffset = r.getEnd(textArea.getBuffer(), physicalLine);
			}
			else
			{
				startOffset = (s.getStart() > lineStart) ? s.getStart() : lineStart;
				endOffset = s.getEnd();
			}
			int tokenStart = lineStart;
			while(token.id != Token.END)
			{
				int next = tokenStart + token.length;
				String sub = null;
				SyntaxStyle style = styles[token.id];
				if (next > startOffset)	
				{
					if (tokenStart >= endOffset)	
						break;
					if(style != null)
					{
						gfx.setFont(style.getFont());
						gfx.setColor(getSelectionFgColor());
						int strStart;
						if (startOffset > tokenStart)
						{
							strStart = startOffset;
							x = nextX(x, style, sub, tokenStart, startOffset);
						}
						else
							strStart = tokenStart; 
						int strEnd = (endOffset > next) ? next : endOffset;
						sub = textArea.getText(strStart, strEnd - strStart);
						gfx.drawString(sub, x, baseLine);
						x = nextX(x, style, sub, strStart, strEnd);
					}
				}
				if (sub == null)
					x = nextX(x, style, sub, tokenStart, next);
				tokenStart = next;
				token = token.next;
			}
		} 

		
		float nextX(float x, SyntaxStyle style, String s, int startOffset,
			int endOffset)
		{
			if (s == null)
				s = textArea.getText(startOffset, endOffset - startOffset);
			if (s.length() == 1 && s.equals("\t"))
				x = nextTabStop(x, endOffset);
			else
			{
				Font font = (style != null) ? style.getFont() : getFont();
				x += font.getStringBounds(s, getFontRenderContext()).getWidth();
			}
			return x;
		}
	} 

	
	private class PaintWrapGuide extends TextAreaExtension
	{
		@Override
		public void paintScreenLineRange(Graphics2D gfx, int firstLine,
			int lastLine, int[] physicalLines, int[] start,
			int[] end, int y, int lineHeight)
		{
			if(textArea.wrapMargin != 0
				&& !textArea.wrapToWidth
				&& isWrapGuidePainted())
			{
				gfx.setColor(getWrapGuideColor());
				int x = textArea.getHorizontalOffset()
					+ textArea.wrapMargin;
				gfx.drawLine(x,y,x,y + (lastLine - firstLine
					+ 1) * lineHeight);
			}
		}

		@Override
		public String getToolTipText(int x, int y)
		{
			if(textArea.wrapMargin != 0
				&& !textArea.wrapToWidth
				&& isWrapGuidePainted())
			{
				int wrapGuidePos = textArea.wrapMargin
					+ textArea.getHorizontalOffset();
				if(Math.abs(x - wrapGuidePos) < 5)
				{
					return String.valueOf(textArea.getBuffer()
						.getProperty("maxLineLen"));
				}
			}

			return null;
		}
	} 

	
	private class PaintText extends TextAreaExtension
	{
		@Override
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			ChunkCache.LineInfo lineInfo = textArea.chunkCache
				.getLineInfo(screenLine);

			Font defaultFont = getFont();
			Color defaultColor = getForeground();

			gfx.setFont(defaultFont);
			gfx.setColor(defaultColor);

			int x = textArea.getHorizontalOffset();
			int originalX = x;

			float baseLine = y + fm.getHeight()
				- (fm.getLeading()+1) - fm.getDescent();

			if(lineInfo.chunks != null)
			{
				x += Chunk.paintChunkList(lineInfo.chunks,
					gfx,textArea.getHorizontalOffset(),
					baseLine,!Debug.DISABLE_GLYPH_VECTOR);
			}

			JEditBuffer buffer = textArea.getBuffer();

			if(!lineInfo.lastSubregion)
			{
				gfx.setFont(defaultFont);
				gfx.setColor(eolMarkerColor);
				gfx.drawString(":",Math.max(x,
					textArea.getHorizontalOffset()
					+ textArea.wrapMargin + textArea.charWidth),
					baseLine);
				x += textArea.charWidth;
			}
			else if(physicalLine < buffer.getLineCount() - 1
				&& buffer.isFoldStart(physicalLine)
				&& !textArea.displayManager
				.isLineVisible(physicalLine + 1))
			{
				int level = buffer.getFoldLevel(physicalLine + 1);
				if(buffer.getFoldHandler() instanceof IndentFoldHandler)
					level = Math.max(1,level / buffer.getIndentSize());
				if(level > 3)
					level = 0;
				SyntaxStyle foldLineStyle = TextAreaPainter.this.foldLineStyle[level];

				Font font = foldLineStyle.getFont();
				gfx.setFont(font);
				gfx.setColor(foldLineStyle.getForegroundColor());

				int nextLine;
				int nextScreenLine = screenLine + 1;
				if(nextScreenLine < textArea.getVisibleLines())
				{
					nextLine = textArea.chunkCache.getLineInfo(nextScreenLine)
						.physicalLine;
				}
				else
				{
					nextLine = textArea.displayManager
						.getNextVisibleLine(physicalLine);
				}

				if(nextLine == -1)
					nextLine = textArea.getLineCount();

				int count = nextLine - physicalLine - 1;
				String str = " [" + count + " lines]";

				float width = getStringWidth(str);

				gfx.drawString(str,x,baseLine);
				x += width;
			}
			else if(eolMarkers)
			{
				gfx.setFont(defaultFont);
				gfx.setColor(eolMarkerColor);
				gfx.drawString(".",x,baseLine);
				x += textArea.charWidth;
			}

			lineInfo.width = x - originalX;
		}
	} 

	
	private class PaintCaret extends TextAreaExtension
	{
		@Override
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			if(!textArea.isCaretVisible())
				return;

			int caret = textArea.getCaretPosition();
			if(caret < start || caret >= end)
				return;

			int offset = caret - textArea.getLineStartOffset(physicalLine);
			textArea.offsetToXY(physicalLine,
					    offset, textArea.offsetXY);
			int caretX = textArea.offsetXY.x;
			int lineHeight = fm.getHeight();

			gfx.setColor(caretColor);

			if(textArea.isOverwriteEnabled())
			{
				gfx.drawLine(caretX,y + lineHeight - 1,
					     caretX + textArea.charWidth,
					     y + lineHeight - 1);
			}
			else if(blockCaret)
				gfx.drawRect(caretX,y,textArea.charWidth - 1,
					     lineHeight - 1);
			else
			{
				if (thickCaret)
					gfx.drawRect(caretX, y,
						1, lineHeight - 1);
				else
					gfx.drawLine(caretX,y,
						caretX,y + lineHeight - 1);
			}
		}
	} 

	
}
