

package org.gjt.sp.jedit.textarea;


import java.awt.*;
import java.awt.event.*;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;

import org.gjt.sp.jedit.buffer.BufferAdapter;
import org.gjt.sp.jedit.buffer.BufferListener;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.util.Log;



public class Gutter extends JComponent implements SwingConstants
{
	
	
	public static final int LOWEST_LAYER = Integer.MIN_VALUE;

	
	public static final int DEFAULT_LAYER = 0;

	
	public static final int HIGHEST_LAYER = Integer.MAX_VALUE;
	

	
	
	public static final String FOLD_PAINTER_PROPERTY = "foldPainter";
	public static final String FOLD_PAINTER_SERVICE = "org.gjt.sp.jedit.textarea.FoldPainter";
	public static final String DEFAULT_FOLD_PAINTER_SERVICE = "Triangle";

	
	public void setFoldPainter(FoldPainter painter)
	{
		if (painter == null)
			foldPainter = new TriangleFoldPainter();
		else
			foldPainter = painter;
	}
	
	
	
	
	
	public Gutter(TextArea textArea)
	{
		this.textArea = textArea;
		enabled = true;
		selectionAreaEnabled = true;
		selectionAreaWidth = SELECTION_GUTTER_WIDTH;

		setAutoscrolls(true);
		setOpaque(true);
		setRequestFocusEnabled(false);

		extensionMgr = new ExtensionManager();

		mouseHandler = new MouseHandler();
		addMouseListener(mouseHandler);
		addMouseMotionListener(mouseHandler);

		bufferListener = new BufferAdapter()
		{
			public void bufferLoaded(JEditBuffer buffer)
			{
				updateLineNumberWidth();
			}

			public void contentInserted(JEditBuffer buffer, int startLine,
					int offset, int numLines, int length)
			{
				updateLineNumberWidth();
			}

			public void contentRemoved(JEditBuffer buffer, int startLine,
					int offset, int numLines, int length) 
			{
				updateLineNumberWidth();
			}
		};

		updateBorder();
		setFoldPainter(textArea.getFoldPainter());
	} 

	
	public void paintComponent(Graphics _gfx)
	{
		Graphics2D gfx = (Graphics2D)_gfx;
		gfx.setRenderingHints(textArea.getPainter().renderingHints);
		
		Rectangle clip = gfx.getClipBounds();
		gfx.setColor(getBackground());
		int bgColorWidth = isSelectionAreaEnabled() ? FOLD_MARKER_SIZE :
			clip.width; 
		gfx.fillRect(clip.x, clip.y, bgColorWidth, clip.height);
		if (isSelectionAreaEnabled())
		{
			if (selectionAreaBgColor == null)
				selectionAreaBgColor = getBackground();
			gfx.setColor(selectionAreaBgColor);
			gfx.fillRect(clip.x + FOLD_MARKER_SIZE, clip.y,
				clip.width - FOLD_MARKER_SIZE, clip.height);
		}
		
		if (textArea.getBuffer().isLoading())
			return;

		int lineHeight = textArea.getPainter().getFontMetrics()
			.getHeight();

		if(lineHeight == 0)
			return;

		int firstLine = clip.y / lineHeight;
		int lastLine = (clip.y + clip.height - 1) / lineHeight;

		if(lastLine - firstLine > textArea.getVisibleLines())
		{
			Log.log(Log.ERROR,this,"BUG: firstLine=" + firstLine);
			Log.log(Log.ERROR,this,"     lastLine=" + lastLine);
			Log.log(Log.ERROR,this,"     visibleLines=" + textArea.getVisibleLines());
			Log.log(Log.ERROR,this,"     height=" + getHeight());
			Log.log(Log.ERROR,this,"     painter.height=" + textArea.getPainter().getHeight());
			Log.log(Log.ERROR,this,"     clip.y=" + clip.y);
			Log.log(Log.ERROR,this,"     clip.height=" + clip.height);
			Log.log(Log.ERROR,this,"     lineHeight=" + lineHeight);
		}
	
		int y = clip.y - clip.y % lineHeight;

		extensionMgr.paintScreenLineRange(textArea,gfx,
			firstLine,lastLine,y,lineHeight);

		for (int line = firstLine; line <= lastLine;
			line++, y += lineHeight)
		{
			paintLine(gfx,line,y);
		}
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
		if(textArea.getBuffer().isLoading())
			return null;

		return extensionMgr.getToolTipText(evt.getX(),evt.getY());
	} 

	
	
	public void setBorder(int width, Color color1, Color color2, Color color3)
	{
		borderWidth = width;

		focusBorder = new CompoundBorder(new MatteBorder(0,0,0,width,color3),
			new MatteBorder(0,0,0,width,color1));
		noFocusBorder = new CompoundBorder(new MatteBorder(0,0,0,width,color3),
			new MatteBorder(0,0,0,width,color2));
		updateBorder();
	} 

	
	
	public void updateBorder()
	{
		if (textArea.hasFocus())
			setBorder(focusBorder);
		else
			setBorder(noFocusBorder);
	} 

	
	
	public void setBorder(Border border)
	{
		super.setBorder(border);

		if (border == null)
		{
			collapsedSize.width = 0;
			collapsedSize.height = 0;
		}
		else
		{
			Insets insets = border.getBorderInsets(this);
			collapsedSize.width = FOLD_MARKER_SIZE + insets.right;
			if (isSelectionAreaEnabled())
				 collapsedSize.width += selectionAreaWidth;
			collapsedSize.height = gutterSize.height
				= insets.top + insets.bottom;
			lineNumberWidth = fm.charWidth('5') * getLineNumberDigitCount(); 
			gutterSize.width = FOLD_MARKER_SIZE + insets.right
				+ lineNumberWidth;
		}

		revalidate();
	} 

	
	public void setMinLineNumberDigitCount(int min)
	{
		if (min == minLineNumberDigits)
			return;
		minLineNumberDigits = min;
		if (textArea.getBuffer() != null)
			updateLineNumberWidth();
	} 

	
	private int getMinLineNumberDigitCount()
	{
		return minLineNumberDigits;
	} 

	
	private int getLineNumberDigitCount()
	{
		JEditBuffer buf = textArea.getBuffer();
		int minDigits = getMinLineNumberDigitCount();
		if (buf == null)
			return minDigits;
		int count = buf.getLineCount();
		int digits;
		for (digits = 0; count > 0; digits++)
			count /= 10;
		return (digits < minDigits) ? minDigits : digits;
	} 

	
	void setBuffer(JEditBuffer newBuffer)
	{
		if (buffer != null)
			buffer.removeBufferListener(bufferListener);
		buffer = newBuffer;
		if (buffer != null)
			buffer.addBufferListener(bufferListener);
		updateLineNumberWidth();
	} 

	
	private void updateLineNumberWidth()
	{
		Font f = getFont();
		if (f != null)
			setFont(getFont());
	} 

	
	void dispose()
	{
		if (buffer != null)
		{
			buffer.removeBufferListener(bufferListener);
			buffer = null;
		}
	} 

	
	
	public void setFont(Font font)
	{
		super.setFont(font);

		fm = getFontMetrics(font);

		Border border = getBorder();
		if(border != null)
		{
			lineNumberWidth = fm.charWidth('5') * getLineNumberDigitCount(); 
			gutterSize.width = FOLD_MARKER_SIZE
				+ border.getBorderInsets(this).right
				+ lineNumberWidth;
			revalidate();
		}
	} 

	

	
	
	public void setGutterEnabled(boolean enabled)
	{
		this.enabled = enabled;
		revalidate();
	} 

	
	public boolean isSelectionAreaEnabled()
	{
		return selectionAreaEnabled;
	} 

	
	public void setSelectionAreaEnabled(boolean enabled)
	{
		if (isSelectionAreaEnabled() == enabled)
			return;
		selectionAreaEnabled = enabled;
		if (enabled)
			collapsedSize.width += selectionAreaWidth;
		else
			collapsedSize.width -= selectionAreaWidth;
		revalidate();
	} 

	
	public void setSelectionAreaBackground(Color bgColor)
	{
		selectionAreaBgColor = bgColor;
		repaint();
	} 

	
	public void setSelectionAreaWidth(int width)
	{
		selectionAreaWidth = width;
		revalidate();
	} 

	
	
	public Color getHighlightedForeground()
	{
		return intervalHighlight;
	} 

	
	public void setHighlightedForeground(Color highlight)
	{
		intervalHighlight = highlight;
	} 

	
	public Color getCurrentLineForeground()
 	{
		return currentLineHighlight;
	} 

	
	public void setCurrentLineForeground(Color highlight)
	{
		currentLineHighlight = highlight;
 	} 

	
	public Color getFoldColor()
 	{
		return foldColor;
	} 

	
	public void setFoldColor(Color foldColor)
	{
		this.foldColor = foldColor;
 	} 

	
	
	public Dimension getPreferredSize()
	{
		if (! enabled)
			return disabledSize;
		if (expanded)
			return gutterSize;
		else
			return collapsedSize;
	} 

	
	public Dimension getMinimumSize()
	{
		return getPreferredSize();
	} 

	
	
	public int getLineNumberAlignment()
	{
		return alignment;
	} 

	
	
	public void setLineNumberAlignment(int alignment)
	{
		if (this.alignment == alignment) return;

		this.alignment = alignment;

		repaint();
	} 

	
	
	public boolean isExpanded()
	{
		return expanded;
	} 

	
	
	public void setExpanded(boolean expanded)
	{
		if (this.expanded == expanded) return;

		this.expanded = expanded;

		textArea.revalidate();
	} 

	
	
	public void toggleExpanded()
	{
		setExpanded(!expanded);
	} 

	
	
	public int getHighlightInterval()
	{
		return interval;
	} 

	
	
	public void setHighlightInterval(int interval)
	{
		if (interval <= 1) interval = 0;
		this.interval = interval;
		repaint();
	} 

	
	public boolean isCurrentLineHighlightEnabled()
	{
		return currentLineHighlightEnabled;
	} 

	
	public void setCurrentLineHighlightEnabled(boolean enabled)
	{
		if (currentLineHighlightEnabled == enabled) return;

		currentLineHighlightEnabled = enabled;

		repaint();
	} 

	
	
	public final Color getStructureHighlightColor()
	{
		return structureHighlightColor;
	} 

	
	
	public final void setStructureHighlightColor(Color structureHighlightColor)
	{
		this.structureHighlightColor = structureHighlightColor;
		repaint();
	} 

	
	
	public final boolean isStructureHighlightEnabled()
	{
		return structureHighlight;
	} 

	
	
	public final void setStructureHighlightEnabled(boolean structureHighlight)
	{
		this.structureHighlight = structureHighlight;
		repaint();
	} 

	public void setSelectionPopupHandler(GutterPopupHandler handler)
	{
		mouseHandler.selectionPopupHandler = handler;
	}

	public void setMouseActionsProvider(MouseActionsProvider mouseActionsProvider)
	{
		mouseHandler.mouseActions = mouseActionsProvider;
	}
	

	

	
	private static final int FOLD_MARKER_SIZE = 12;
	private static final int SELECTION_GUTTER_WIDTH = 12;
		

	private boolean enabled;
	private final TextArea textArea;
	private MouseHandler mouseHandler;
	private ExtensionManager extensionMgr;

	private Dimension gutterSize = new Dimension(0,0);
	private Dimension collapsedSize = new Dimension(0,0);
	private int lineNumberWidth;
	private Dimension disabledSize = new Dimension(0,0);

	private Color intervalHighlight;
	private Color currentLineHighlight;
	private Color foldColor;
	private Color selectionAreaBgColor;

	private FontMetrics fm;

	private int alignment;

	private int interval;
	private boolean currentLineHighlightEnabled;
	private boolean expanded;
	private boolean selectionAreaEnabled;

	private boolean structureHighlight;
	private Color structureHighlightColor;

	private int borderWidth;
	private Border focusBorder, noFocusBorder;
	
	private FoldPainter foldPainter;
	private JEditBuffer buffer;
	private BufferListener bufferListener;
	private int minLineNumberDigits;
	private int selectionAreaWidth;
	

	
	private void paintLine(Graphics2D gfx, int line, int y)
	{
		JEditBuffer buffer = textArea.getBuffer();
		if(buffer.isLoading())
			return;

		FontMetrics textAreaFm = textArea.getPainter().getFontMetrics();
		int lineHeight = textAreaFm.getHeight();
		int baseline = textAreaFm.getAscent();

		ChunkCache.LineInfo info = textArea.chunkCache.getLineInfo(line);
		int physicalLine = info.physicalLine;

		
		if(physicalLine == -1)
			return;

		boolean drawFoldMiddle = true;
		
		if(info.firstSubregion && buffer.isFoldStart(physicalLine))
		{
			drawFoldMiddle = false;
			foldPainter.paintFoldStart(this, gfx, line, physicalLine,
					textArea.displayManager.isLineVisible(physicalLine+1),
					y, lineHeight, buffer);
		}
		else if(info.lastSubregion && buffer.isFoldEnd(physicalLine))
		{
			drawFoldMiddle = false;
			foldPainter.paintFoldEnd(this, gfx, line, physicalLine, y,
					lineHeight, buffer);
		} 
		
		else if(structureHighlight)
		{
			StructureMatcher.Match match = textArea.getStructureMatch();
			int caretLine = textArea.getCaretLine();

			if(textArea.isStructureHighlightVisible()
				&& physicalLine >= Math.min(caretLine,match.startLine)
				&& physicalLine <= Math.max(caretLine,match.startLine))
			{
				int caretScreenLine;
				if(caretLine > textArea.getLastPhysicalLine())
					caretScreenLine = Integer.MAX_VALUE;
				else if(textArea.displayManager.isLineVisible(
						textArea.getCaretLine()))
				{
					caretScreenLine = textArea
						.getScreenLineOfOffset(
						textArea.getCaretPosition());
				}
				else
				{
					caretScreenLine = -1;
				}

				int structScreenLine;
				if(match.startLine > textArea.getLastPhysicalLine())
					structScreenLine = Integer.MAX_VALUE;
				else if(textArea.displayManager.isLineVisible(
						match.startLine))
				{
					structScreenLine = textArea
						.getScreenLineOfOffset(
						match.start);
				}
				else
				{
					structScreenLine = -1;
				}

				if(caretScreenLine > structScreenLine)
				{
					int tmp = caretScreenLine;
					caretScreenLine = structScreenLine;
					structScreenLine = tmp;
				}

				gfx.setColor(structureHighlightColor);
				drawFoldMiddle = false;
				if(structScreenLine == caretScreenLine)
				{
					
					drawFoldMiddle = true;
				}
				
				else if(line == caretScreenLine)
				{
					gfx.fillRect(5,
						y
						+ lineHeight / 2,
						5,
						2);
					gfx.fillRect(5,
						y
						+ lineHeight / 2,
						2,
						lineHeight - lineHeight / 2);
				}
				
				else if(line == structScreenLine)
				{
					gfx.fillRect(5,
						y,
						2,
						lineHeight / 2);
					gfx.fillRect(5,
						y + lineHeight / 2,
						5,
						2);
				}
				
				else if(line > caretScreenLine
					&& line < structScreenLine)
				{
					gfx.fillRect(5,
						y,
						2,
						lineHeight);
				}
			}
		} 
		if(drawFoldMiddle && buffer.getFoldLevel(physicalLine) > 0)
		{
			foldPainter.paintFoldMiddle(this, gfx, line, physicalLine,
					y, lineHeight, buffer);
		}

		
		if(info.firstSubregion && expanded)
		{
			String number = Integer.toString(physicalLine + 1);

			int offset;
			switch (alignment)
			{
			case RIGHT:
				offset = lineNumberWidth - (fm.stringWidth(number) + 1);
				break;
			case CENTER:
				offset = (lineNumberWidth - fm.stringWidth(number)) / 2;
				break;
			case LEFT: default:
				offset = 0;
				break;
			}

			if (physicalLine == textArea.getCaretLine() && currentLineHighlightEnabled)
			{
				gfx.setColor(currentLineHighlight);
			}
			else if (interval > 1 && (physicalLine + 1) % interval == 0)
				gfx.setColor(intervalHighlight);
			else
				gfx.setColor(getForeground());

			gfx.drawString(number, FOLD_MARKER_SIZE + offset,
				baseline + y);
		} 
	} 

	

	
	class MouseHandler extends MouseInputAdapter
	{
		MouseActionsProvider mouseActions;
		boolean drag;
		int toolTipInitialDelay, toolTipReshowDelay;
		boolean selectLines;
		int selAnchorLine;
		GutterPopupHandler selectionPopupHandler;

		
		public void mouseEntered(MouseEvent e)
		{
			ToolTipManager ttm = ToolTipManager.sharedInstance();
			toolTipInitialDelay = ttm.getInitialDelay();
			toolTipReshowDelay = ttm.getReshowDelay();
			ttm.setInitialDelay(0);
			ttm.setReshowDelay(0);
		} 

		
		public void mouseExited(MouseEvent evt)
		{
			ToolTipManager ttm = ToolTipManager.sharedInstance();
			ttm.setInitialDelay(toolTipInitialDelay);
			ttm.setReshowDelay(toolTipReshowDelay);
		} 

		
		public void mousePressed(MouseEvent e)
		{
			textArea.requestFocus();

			boolean outsideGutter =
				(e.getX() >= getWidth() - borderWidth * 2);
			if(TextAreaMouseHandler.isPopupTrigger(e) || outsideGutter)
			{
				if ((selectionPopupHandler != null) &&
					(! outsideGutter) &&
					(e.getX() > FOLD_MARKER_SIZE))
				{
					int screenLine = e.getY() / textArea.getPainter()
						.getFontMetrics().getHeight();
					int line = textArea.chunkCache.getLineInfo(screenLine)
						.physicalLine;
					if (line >= 0)
					{
						selectionPopupHandler.handlePopup(
							e.getX(), e.getY(), line);
						return;
					}
				}
				e.translatePoint(-getWidth(),0);
				textArea.mouseHandler.mousePressed(e);
				drag = true;
			}
			else
			{
				JEditBuffer buffer = textArea.getBuffer();

				int screenLine = e.getY() / textArea.getPainter()
					.getFontMetrics().getHeight();

				int line = textArea.chunkCache.getLineInfo(screenLine)
					.physicalLine;

				if(line == -1)
					return;

				if (e.getX() >= FOLD_MARKER_SIZE)
				{
					Selection s = new Selection.Range(
						textArea.getLineStartOffset(line),
						getFoldEndOffset(line));
					if(textArea.isMultipleSelectionEnabled())
						textArea.addToSelection(s);
					else
						textArea.setSelection(s);
					selectLines = true;
					selAnchorLine = line;
					return;
				}

				
				String defaultAction;
				String variant;
				if(buffer.isFoldStart(line))
				{
					defaultAction = "toggle-fold";
					variant = "fold";
				}
				else if(structureHighlight
					&& textArea.isStructureHighlightVisible()
					&& textArea.lineInStructureScope(line))
				{
					defaultAction = "match-struct";
					variant = "struct";
				}
				else
					return;

				String action = null;

				if (mouseActions != null)
					action = mouseActions.getActionForEvent(
						e,variant);

				if(action == null)
					action = defaultAction;
				

				
				StructureMatcher.Match match = textArea
					.getStructureMatch();

				if(action.equals("select-fold"))
				{
					textArea.displayManager.expandFold(line,true);
					textArea.selectFold(line);
				}
				else if(action.equals("narrow-fold"))
				{
					int[] lines = buffer.getFoldAtLine(line);
					textArea.displayManager.narrow(lines[0],lines[1]);
				}
				else if(action.startsWith("toggle-fold"))
				{
					if(textArea.displayManager
						.isLineVisible(line + 1))
					{
						textArea.collapseFold(line);
					}
					else
					{
						if(action.endsWith("-fully"))
						{
							textArea.displayManager
								.expandFold(line,
								true);
						}
						else
						{
							textArea.displayManager
								.expandFold(line,
								false);
						}
					}
				}
				else if(action.equals("match-struct"))
				{
					if(match != null)
						textArea.setCaretPosition(match.end);
				}
				else if(action.equals("select-struct"))
				{
					if(match != null)
					{
						match.matcher.selectMatch(
							textArea);
					}
				}
				else if(action.equals("narrow-struct"))
				{
					if(match != null)
					{
						int start = Math.min(
							match.startLine,
							textArea.getCaretLine());
						int end = Math.max(
							match.endLine,
							textArea.getCaretLine());
						textArea.displayManager.narrow(start,end);
					}
				} 
			}
		} 

		
		public void mouseDragged(MouseEvent e)
		{
			if(drag )
			{
				e.translatePoint(-getWidth(),0);
				textArea.mouseHandler.mouseDragged(e);
			}
			else if(selectLines)
			{
				int screenLine = e.getY() / textArea.getPainter()
					.getFontMetrics().getHeight();
				int line;
				if(e.getY() < 0)
				{
					textArea.scrollUpLine();
					line = textArea.getFirstPhysicalLine();
				}
				else if(e.getY() >= getHeight())
				{
					textArea.scrollDownLine();
					line = textArea.getLastPhysicalLine();
				}
				else
					line = textArea.chunkCache.getLineInfo(screenLine)
						.physicalLine;

				int selStart, selEnd;
				if(line < selAnchorLine)
				{
					selStart = textArea.getLineStartOffset(line);
					selEnd = getFoldEndOffset(selAnchorLine);
				}
				else
				{
					selStart = textArea.getLineStartOffset(selAnchorLine);
					selEnd = getFoldEndOffset(line);
				}

				textArea.resizeSelection(selStart, selEnd, 0, false);
			}
		} 

		
		private int getFoldEndOffset(int line)
		{
			JEditBuffer buffer = textArea.getBuffer();
			int endLine;
			if ((line == buffer.getLineCount() - 1) ||
				(textArea.displayManager.isLineVisible(line + 1)))
			{
				endLine = line;
			}
			else
			{
				int[] lines = buffer.getFoldAtLine(line);
				endLine = lines[1];
			}

			if(endLine == buffer.getLineCount() - 1)
				return buffer.getLineEndOffset(endLine) - 1;
			else
				return buffer.getLineEndOffset(endLine);
		} 

		
		public void mouseReleased(MouseEvent e)
		{
			if(drag && e.getX() >= getWidth() - borderWidth * 2)
			{
				e.translatePoint(-getWidth(),0);
				textArea.mouseHandler.mouseReleased(e);
			}

			drag = false;
			selectLines = false;
		} 
	} 
}
