

package org.gjt.sp.jedit.textarea;


import javax.swing.text.*;
import javax.swing.JComponent;
import java.awt.event.MouseEvent;
import java.awt.font.*;
import java.awt.*;
import java.util.HashMap;
import org.gjt.sp.jedit.buffer.IndentFoldHandler;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.Buffer;
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
	

	
	
	public void setBounds(int x, int y, int width, int height)
	{
		if(x == getX() && y == getY() && width == getWidth()
			&& height == getHeight())
		{
			return;
		}

		super.setBounds(x,y,width,height);

		textArea.recalculateVisibleLines();
		if(textArea.getBuffer().isLoaded())
			textArea.recalculateLastPhysicalLine();
		textArea.propertiesChanged();
		textArea.scrollBarsInitialized = true;
	} 

	
	
	public boolean isManagingFocus()
	{
		return false;
	} 

	
	
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
		for(int i = 0; i < styles.length; i++)
		{
			styles[i].setCharWidth(getCharWidth(styles[i].getFont()));
		}
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
		if(textArea.getBuffer() != null)
			textArea.invalidateSelectedLines();
	} 

	
	
	public final Color getMultipleSelectionColor()
	{
		return multipleSelectionColor;
	} 

	
	
	public final void setMultipleSelectionColor(Color multipleSelectionColor)
	{
		this.multipleSelectionColor = multipleSelectionColor;
		if(textArea.getBuffer() != null)
			textArea.invalidateSelectedLines();
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
		if(textArea.getBuffer() != null)
			textArea.invalidateSelectedLines();
	} 

	
	
	public final Color getStructureHighlightColor()
	{
		return structureHighlightColor;
	} 

	
	
	public final void setStructureHighlightColor(
		Color structureHighlightColor)
	{
		this.structureHighlightColor = structureHighlightColor;
		StructureMatcher.Match match = textArea.getStructureMatch();
		if(match != null)
		{
			textArea.invalidateLineRange(
				match.startLine,match.endLine
			);
		}
	} 

	
	
	public final boolean isStructureHighlightEnabled()
	{
		return structureHighlight;
	} 

	
	
	public final void setStructureHighlightEnabled(boolean structureHighlight)
	{
		this.structureHighlight = structureHighlight;
		StructureMatcher.Match match = textArea.getStructureMatch();
		if(match != null)
		{
			textArea.invalidateLineRange(
				match.startLine,
				match.endLine
			);
		}
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

	
	
	public void setAntiAliasEnabled(boolean antiAlias)
	{
		this.antiAlias = antiAlias;
		updateRenderingHints();
	} 

	
	
	public boolean isAntiAliasEnabled()
	{
		return antiAlias;
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

	
	
	public String getToolTipText(MouseEvent evt)
	{
		if(!textArea.getBuffer().isLoaded())
			return null;

		return extensionMgr.getToolTipText(evt.getX(),evt.getY());
	} 

	
	
	public FontMetrics getFontMetrics()
	{
		return fm;
	} 

	
	
	public void setFont(Font font)
	{
		super.setFont(font);
		fm = getFontMetrics(font);
		textArea.recalculateVisibleLines();
		textArea.propertiesChanged();
	} 

	
	
	public void paintComponent(Graphics _gfx)
	{
		Graphics2D gfx = (Graphics2D)_gfx;
		gfx.setRenderingHints(renderingHints);
		fontRenderContext = gfx.getFontRenderContext();

		Rectangle clipRect = gfx.getClipBounds();

		gfx.setColor(getBackground());
		gfx.fillRect(clipRect.x,clipRect.y,clipRect.width,clipRect.height);

		Buffer buffer = textArea.getBuffer();
		if(!buffer.isLoaded())
			return;

		int x = textArea.getHorizontalOffset();

		int height = fm.getHeight();
		int firstInvalid = clipRect.y / height;
		
		
		
		int lastInvalid = (clipRect.y + clipRect.height - 1) / height;

		if(Debug.PAINT_TIMER && lastInvalid - firstInvalid >= 1)
			Log.log(Log.DEBUG,this,"repainting " + (lastInvalid - firstInvalid) + " lines");

		int y = (clipRect.y - clipRect.y % height);

		extensionMgr.paintScreenLineRange(textArea,gfx,
			firstInvalid,lastInvalid,y,height);

		textArea.updateMaxHorizontalScrollWidth();
		textArea.displayManager._notifyScreenLineChanges();
	} 

	
	
	public float nextTabStop(float x, int tabOffset)
	{
		int ntabs = (int)(x / textArea.tabSize);
		return (ntabs + 1) * textArea.tabSize;
	} 

	
	
	public Dimension getPreferredSize()
	{
		Dimension dim = new Dimension();

		char[] foo = new char[80];
		for(int i = 0; i < foo.length; i++)
			foo[i] = ' ';
		dim.width = (int)(getFont().getStringBounds(foo,0,foo.length,
			fontRenderContext).getWidth());
		dim.height = fm.getHeight() * 25;
		return dim;
	} 

	
	
	public Dimension getMinimumSize()
	{
		return getPreferredSize();
	} 

	

	
	
	JEditTextArea textArea;

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
	boolean lineHighlight;
	boolean structureHighlight;
	boolean eolMarkers;
	boolean wrapGuide;
	boolean antiAlias;
	boolean fracFontMetrics;

	
	FontMetrics fm;
	

	
	
	TextAreaPainter(JEditTextArea textArea)
	{
		enableEvents(AWTEvent.FOCUS_EVENT_MASK
			| AWTEvent.KEY_EVENT_MASK
			| AWTEvent.MOUSE_EVENT_MASK);

		this.textArea = textArea;

		fonts = new HashMap();
		extensionMgr = new ExtensionManager();

		setAutoscrolls(true);
		setOpaque(true);
		setRequestFocusEnabled(false);

		setCursor(Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR));

		fontRenderContext = new FontRenderContext(null,false,false);

		addExtension(LINE_BACKGROUND_LAYER,new PaintLineBackground());
		addExtension(SELECTION_LAYER,new PaintSelection());
		addExtension(WRAP_GUIDE_LAYER,new PaintWrapGuide());
		addExtension(BRACKET_HIGHLIGHT_LAYER,new StructureMatcher
			.Highlight(textArea));
		addExtension(TEXT_LAYER,new PaintText());
		caretExtension = new PaintCaret();
	} 

	

	

	
	private ExtensionManager extensionMgr;
	private PaintCaret caretExtension;
	private RenderingHints renderingHints;
	private FontRenderContext fontRenderContext;
	private HashMap fonts;
	

	
	private void updateRenderingHints()
	{
		HashMap hints = new HashMap();

		if(antiAlias)
		{
			
			hints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
			hints.put(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
		}
		else
		{
			hints.put(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_OFF);
			hints.put(RenderingHints.KEY_TEXT_ANTIALIASING, RenderingHints.VALUE_TEXT_ANTIALIAS_OFF);
		}

		hints.put(RenderingHints.KEY_FRACTIONALMETRICS,
			fracFontMetrics ?
				RenderingHints.VALUE_FRACTIONALMETRICS_ON
				: RenderingHints.VALUE_FRACTIONALMETRICS_OFF);

		renderingHints = new RenderingHints(hints);
		fontRenderContext = new FontRenderContext(null,antiAlias,
			fracFontMetrics);
	} 

	
	private int getCharWidth(Font font)
	{
		Integer returnValue = (Integer)fonts.get(font);
		if(returnValue == null)
		{
			int minWidth = Integer.MAX_VALUE;
			int maxWidth = Integer.MIN_VALUE;
			FontMetrics fm = getFontMetrics(font);
			int[] widths = fm.getWidths();
			for(int i = 0; i < widths.length; i++)
			{
				int width = widths[i];
				if(width == 0 || !font.canDisplay((char)i))
					continue;
				minWidth = Math.min(width,minWidth);
				maxWidth = Math.max(width,maxWidth);
			}

			String str = "iwiwiwiau1234";
			double width1 = font.createGlyphVector(textArea.getPainter()
				.getFontRenderContext(),str).getLogicalBounds()
				.getWidth();
			double width2 = str.length() * maxWidth;
			if(minWidth == maxWidth
				&& width1 == width2)
			{
				Log.log(Log.DEBUG,this,"Using monospaced font optimization: " + font);
				returnValue = new Integer(maxWidth);
			}
			else
			{
				Log.log(Log.DEBUG,this,"Not using monospaced font optimization: " + font);
				Log.log(Log.DEBUG,this,"Minimum width = " + minWidth
					+ ", maximum width = " + maxWidth);
				returnValue = new Integer(0);
			}

			fonts.put(font,returnValue);
		}
		return returnValue.intValue();
	} 

	

	

	
	class PaintLineBackground extends TextAreaExtension
	{
		
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			
			JEditTextArea textArea = TextAreaPainter.this.textArea;
			Buffer buffer = textArea.getBuffer();

			
			boolean collapsedFold =
				(physicalLine < buffer.getLineCount() - 1
				&& buffer.isFoldStart(physicalLine)
				&& !textArea.displayManager
				.isLineVisible(physicalLine + 1));

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
			boolean paintLineHighlight = isLineHighlightEnabled()
				&& caret >= start && caret < end
				&& textArea.selection.size() == 0;

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
					- fm.getLeading() - fm.getDescent();
				Chunk.paintChunkBackgrounds(
					lineInfo.chunks,gfx,
					textArea.getHorizontalOffset(),
					baseLine);
			} 
		} 
	} 

	
	class PaintSelection extends TextAreaExtension
	{
		
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			if(textArea.selection.size() == 0)
				return;

			gfx.setColor(textArea.isMultipleSelectionEnabled()
				? getMultipleSelectionColor()
				: getSelectionColor());
			for(int i = textArea.selection.size() - 1; i >= 0; i--)
			{
				paintSelection(gfx,screenLine,physicalLine,y,
					(Selection)textArea.selection.get(i));
			}
		} 

		
		private void paintSelection(Graphics2D gfx, int screenLine,
			int physicalLine, int y, Selection s)
		{
			int[] selectionStartAndEnd
				= textArea.getSelectionStartAndEnd(
				screenLine,physicalLine,s);
			if(selectionStartAndEnd == null)
				return;

			int x1 = selectionStartAndEnd[0];
			int x2 = selectionStartAndEnd[1];

			gfx.fillRect(x1,y,x2 - x1,fm.getHeight());
		} 
	} 

	
	class PaintWrapGuide extends TextAreaExtension
	{
		public void paintScreenLineRange(Graphics2D gfx, int firstLine,
			int lastLine, int[] physicalLines, int[] start,
			int[] end, int y, int lineHeight)
		{
			if(textArea.getDisplayManager().wrapMargin != 0
				&& isWrapGuidePainted())
			{
				gfx.setColor(getWrapGuideColor());
				int x = textArea.getHorizontalOffset()
					+ textArea.getDisplayManager()
					.wrapMargin;
				gfx.drawLine(x,y,x,y + (lastLine - firstLine
					+ 1) * lineHeight);
			}
		}

		public String getToolTipText(int x, int y)
		{
			if(textArea.getDisplayManager().wrapMargin != 0 && isWrapGuidePainted())
			{
				int wrapGuidePos = textArea.getDisplayManager().wrapMargin
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

	
	class PaintText extends TextAreaExtension
	{
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
				- fm.getLeading() - fm.getDescent();

			if(lineInfo.chunks != null)
			{
				x += Chunk.paintChunkList(lineInfo.chunks,
					gfx,textArea.getHorizontalOffset(),
					baseLine,!Debug.DISABLE_GLYPH_VECTOR);
			}

			Buffer buffer = textArea.getBuffer();

			if(!lineInfo.lastSubregion)
			{
				gfx.setFont(defaultFont);
				gfx.setColor(eolMarkerColor);
				gfx.drawString(":",Math.max(x,
					textArea.getHorizontalOffset()
					+ textArea.getDisplayManager().wrapMargin + textArea.charWidth),
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

				float width = (float)font.getStringBounds(
					str,fontRenderContext).getWidth();

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

			lineInfo.width = (x - originalX);
		}
	} 

	
	class PaintCaret extends TextAreaExtension
	{
		public void paintValidLine(Graphics2D gfx, int screenLine,
			int physicalLine, int start, int end, int y)
		{
			if(!textArea.isCaretVisible())
				return;

			int caret = textArea.getCaretPosition();
			if(caret < start || caret >= end)
				return;

			int offset = caret - textArea.getLineStartOffset(physicalLine);
			textArea.offsetToXY(physicalLine,offset,textArea.returnValue);
			int caretX = textArea.returnValue.x;
			int height = fm.getHeight();

			gfx.setColor(caretColor);

			if(textArea.isOverwriteEnabled())
			{
				gfx.drawLine(caretX,y + height - 1,
					caretX + textArea.charWidth,y + height - 1);
			}
			else if(blockCaret)
				gfx.drawRect(caretX,y,textArea.charWidth - 1,height - 1);
			else
				gfx.drawLine(caretX,y,caretX,y + height - 1);
		}
	} 

	
}
