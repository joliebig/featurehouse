
package org.gjt.sp.jedit.textarea;

import org.gjt.sp.jedit.input.InputHandlerProvider;
import org.gjt.sp.jedit.input.AbstractInputHandler;
import org.gjt.sp.jedit.input.DefaultInputHandlerProvider;
import org.gjt.sp.jedit.input.TextAreaInputHandler;
import org.gjt.sp.jedit.syntax.Chunk;
import org.gjt.sp.jedit.syntax.TokenMarker;
import org.gjt.sp.jedit.syntax.ParserRuleSet;
import org.gjt.sp.jedit.syntax.SyntaxStyle;
import org.gjt.sp.jedit.buffer.JEditBuffer;
import org.gjt.sp.jedit.Debug;
import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;

import javax.swing.event.EventListenerList;
import javax.swing.event.MouseInputAdapter;
import javax.swing.event.CaretListener;
import javax.swing.event.CaretEvent;
import javax.swing.text.Segment;
import javax.swing.*;
import javax.swing.Timer;
import javax.swing.plaf.metal.MetalLookAndFeel;
import java.awt.*;
import java.awt.im.InputMethodRequests;
import java.awt.event.*;
import java.util.*;


public class TextArea extends JComponent
{
	
	public TextArea()
	{
		this(null);
		inputHandlerProvider = new DefaultInputHandlerProvider(new TextAreaInputHandler(this));
		setMouseHandler(new TextAreaMouseHandler(this));
		Font font1 = new Font("Monospaced", Font.PLAIN, 12);
		painter.setFont(font1);
		SyntaxStyle[] styles = new SyntaxStyle[1];
		styles[0] = new SyntaxStyle(Color.black, Color.white, font1);
		painter.setStyles(styles);
		painter.setBlockCaretEnabled(false);



		painter.setStructureHighlightEnabled(true);
		painter.setStructureHighlightColor(Color.black);
		painter.setEOLMarkersPainted(false);
		painter.setEOLMarkerColor(new Color(255,102,51));
		painter.setWrapGuidePainted(true);
		painter.setWrapGuideColor(new Color(125,125,255));
		painter.setCaretColor(Color.red);
		painter.setSelectionColor(new Color(204,204,255));
		painter.setMultipleSelectionColor(new Color(204,255,204));
		painter.setBackground(Color.white);
		painter.setForeground(Color.black);
		painter.setBlockCaretEnabled(false);
		painter.setLineHighlightEnabled(true);
		painter.setLineHighlightColor(new Color(255,204,255));
		painter.setAntiAlias(new AntiAlias(0));
		painter.setFractionalFontMetricsEnabled(false);


		gutter.setExpanded(false);
		gutter.setHighlightInterval(5);
		gutter.setCurrentLineHighlightEnabled(true);
		gutter.setStructureHighlightEnabled(true);
		gutter.setStructureHighlightColor(new Color(102,102,153));
		gutter.setBackground(new Color(219,219,219));
		gutter.setForeground(Color.black);
		gutter.setHighlightedForeground(new Color(153,0,102));
		gutter.setFoldColor(new Color(131,131,131));
		gutter.setCurrentLineForeground(new Color(255,0,51));
		gutter.setLineNumberAlignment(Gutter.RIGHT);
		gutter.setFont(new Font("Monospaced", Font.PLAIN, 10));
		gutter.setBorder(3, new Color(153,0,153), Color.white, painter.getBackground());

		setCaretBlinkEnabled(true);
		setElectricScroll(3);

		JEditBuffer buffer = new JEditBuffer();
		TokenMarker tokenMarker = new TokenMarker();
		tokenMarker.addRuleSet(new ParserRuleSet("text","MAIN"));
		buffer.setTokenMarker(tokenMarker);
		buffer.insert(0,"ahaha coucou\ncaca");
		setBuffer(buffer);
	} 

	
	
	public TextArea(InputHandlerProvider inputHandlerProvider)
	{
		this.inputHandlerProvider = inputHandlerProvider;
		enableEvents(AWTEvent.FOCUS_EVENT_MASK | AWTEvent.KEY_EVENT_MASK);

		
		selectionManager = new SelectionManager(this);
		chunkCache = new ChunkCache(this);
		painter = new TextAreaPainter(this);
		repaintMgr = new FastRepaintManager(this,painter);
		gutter = new Gutter(this);
		gutter.setMouseActionsProvider(new MouseActions("gutter"));
		listenerList = new EventListenerList();
		caretEvent = new MutableCaretEvent();
		blink = true;
		offsetXY = new Point();
		structureMatchers = new LinkedList<StructureMatcher>();
		structureMatchers.add(new StructureMatcher.BracketMatcher());
		

		
		setLayout(new ScrollLayout());
		add(ScrollLayout.CENTER,painter);
		add(ScrollLayout.LEFT,gutter);

		
		verticalBox = new Box(BoxLayout.X_AXIS);
		verticalBox.add(vertical = new JScrollBar(Adjustable.VERTICAL));
		vertical.setRequestFocusEnabled(false);
		add(ScrollLayout.RIGHT,verticalBox);
		add(ScrollLayout.BOTTOM,
			horizontal = new JScrollBar(Adjustable.HORIZONTAL));
		horizontal.setRequestFocusEnabled(false);

		horizontal.setValues(0,0,0,0);
		

		
		
		
		
		
		if(UIManager.getLookAndFeel() instanceof MetalLookAndFeel)
		{
			setBorder(new TextAreaBorder());
			vertical.putClientProperty("JScrollBar.isFreeStanding",
				Boolean.FALSE);
			horizontal.putClientProperty("JScrollBar.isFreeStanding",
				Boolean.FALSE);
			
		}
		

		
		vertical.addAdjustmentListener(new AdjustHandler());
		horizontal.addAdjustmentListener(new AdjustHandler());


		addFocusListener(new FocusHandler());
		addMouseWheelListener(new MouseWheelHandler());

		

		
		
		
		focusedComponent = this;

	} 

	
	public void setMouseHandler(MouseInputAdapter mouseInputAdapter)
	{
		mouseHandler = mouseInputAdapter;
		painter.addMouseListener(mouseHandler);
		painter.addMouseMotionListener(mouseHandler);
	} 

	
	public void setTransferHandler(TransferHandler newHandler)
	{
		super.setTransferHandler(newHandler);
		try
		{
			getDropTarget().addDropTargetListener(
				new TextAreaDropHandler(this));
		}
		catch(TooManyListenersException e)
		{
			Log.log(Log.ERROR,this,e);
		}
	} 

	
	public String toString() {
		StringBuilder builder = new StringBuilder();
		builder.append("caret: ").append(caret).append('\n');
		builder.append("caretLine: ").append(caretLine).append('\n');
		builder.append("caretScreenLine: ").append(caretScreenLine).append('\n');
		builder.append("electricScroll: ").append(electricScroll).append('\n');
		builder.append("horizontalOffset: ").append(horizontalOffset).append('\n');
		builder.append("magicCaret: ").append(magicCaret).append('\n');
		builder.append("offsetXY").append(offsetXY.toString()).append('\n');
		builder.append("oldCaretLine: ").append(oldCaretLine).append('\n');
		builder.append("screenLastLine: ").append(screenLastLine).append('\n');
		builder.append("visibleLines: ").append(visibleLines).append('\n');
		builder.append("firstPhysicalLine: ").append(getFirstPhysicalLine()).append('\n');
		builder.append("physLastLine: ").append(physLastLine).append('\n');
		return builder.toString();
	} 

	
	
	public void dispose()
	{
		DisplayManager.textAreaDisposed(this);
	} 

	
	
	public AbstractInputHandler getInputHandler()
	{
		return inputHandlerProvider.getInputHandler();
	} 

	
	
	public final TextAreaPainter getPainter()
	{
		return painter;
	} 

	
 	
	public final Gutter getGutter()
	{
		return gutter;
	} 

	
	
	public DisplayManager getDisplayManager()
	{
		return displayManager;
	} 

	
	
	public final boolean isCaretBlinkEnabled()
	{
		return caretBlinks;
	} 

	
	
	public void setCaretBlinkEnabled(boolean caretBlinks)
	{
		this.caretBlinks = caretBlinks;
		if(!caretBlinks)
			blink = false;

		if(buffer != null)
			invalidateLine(caretLine);
	} 

	

	
	public final int getElectricScroll()
	{
		return electricScroll;
	} 

	
	
	public final void setElectricScroll(int electricScroll)
	{
		this.electricScroll = electricScroll;
	} 

	
	
	public final boolean isQuickCopyEnabled()
	{
		return quickCopy;
	} 

	
	
	public final void setQuickCopyEnabled(boolean quickCopy)
	{
		this.quickCopy = quickCopy;
	} 

	
	
	public final JEditBuffer getBuffer()
	{
		return buffer;
	} 

	
	
	public void setBuffer(JEditBuffer buffer)
	{
		if(this.buffer == buffer)
			return;

		try
		{
			bufferChanging = true;

			if(this.buffer != null)
			{
				
				

				if(!buffer.isLoading())
					selectNone();
				caretLine = caret = caretScreenLine = 0;
				match = null;
			}
			boolean inCompoundEdit = false;
			if (this.buffer != null)
				inCompoundEdit = this.buffer.insideCompoundEdit();
			if (inCompoundEdit)
				this.buffer.endCompoundEdit();
			this.buffer = buffer;
			if (inCompoundEdit)
				this.buffer.beginCompoundEdit();

			chunkCache.setBuffer(buffer);
			repaintMgr.setFastScroll(false);
			propertiesChanged();

			if(displayManager != null)
			{
				DisplayManager.releaseDisplayManager(
					displayManager);
			}

			displayManager = DisplayManager.getDisplayManager(
				buffer,this);

			displayManager.init();

			if(buffer.isLoading())
				updateScrollBar();

			repaint();

			fireScrollEvent(true);
		}
		finally
		{
			bufferChanging = false;
		}
	} 

	
	
	public final boolean isEditable()
	{
		return buffer.isEditable();
	} 

	
	
	public boolean isDragInProgress()
	{
		return dndInProgress;
	} 

	
	
	public void setDragInProgress(boolean dndInProgress)
	{
		this.dndInProgress = dndInProgress;
	} 

	
	
	public boolean isDragEnabled()
	{
		return dndEnabled;
	} 

	
	
	public void setDragEnabled(boolean dndEnabled)
	{
		this.dndEnabled = dndEnabled;
	} 

	
	
	public boolean getJoinNonWordChars()
	{
		return joinNonWordChars;
	} 

	
	
	public void setJoinNonWordChars(boolean joinNonWordChars)
	{
		this.joinNonWordChars = joinNonWordChars;
	} 

	

	
	
	public final int getFirstLine()
	{
		return displayManager.firstLine.scrollLine
			+ displayManager.firstLine.skew;
	} 

	
	
	public void setFirstLine(int firstLine)
	{
		
		int max = displayManager.getScrollLineCount() - visibleLines
			+ (lastLinePartial ? 1 : 0);
		if(firstLine > max)
			firstLine = max;
		if(firstLine < 0)
			firstLine = 0;
		

		if(Debug.SCROLL_DEBUG)
		{
			Log.log(Log.DEBUG,this,"setFirstLine() from "
				+ getFirstLine() + " to " + firstLine);
		}

		int oldFirstLine = getFirstLine();
		if(firstLine == oldFirstLine)
			return;

		displayManager.setFirstLine(oldFirstLine,firstLine);

		repaint();

		fireScrollEvent(true);
	} 

	
	
	public final int getFirstPhysicalLine()
	{
		return displayManager.firstLine.physicalLine;
	} 

	
	
	public void setFirstPhysicalLine(int physFirstLine)
	{
		setFirstPhysicalLine(physFirstLine,0);
	} 

	
	
	public void setFirstPhysicalLine(int physFirstLine, int skew)
	{
		if(Debug.SCROLL_DEBUG)
		{
			Log.log(Log.DEBUG,this,"setFirstPhysicalLine("
				+ physFirstLine + ',' + skew + ')');
		}

		int amount = physFirstLine - displayManager.firstLine.physicalLine;

		displayManager.setFirstPhysicalLine(amount,skew);

		repaint();

		fireScrollEvent(true);
	} 

	
	
	public final int getLastPhysicalLine()
	{
		return physLastLine;
	} 

	
	
	public int getLastScreenLine()
	{
		return screenLastLine;
	} 

	
	
	public final int getVisibleLines()
	{
		return visibleLines;
	} 

	
	
	public final int getHorizontalOffset()
	{
		return horizontalOffset;
	} 

	
	
	public void setHorizontalOffset(int horizontalOffset)
	{
		if(horizontalOffset > 0)
			horizontalOffset = 0;

		if(horizontalOffset == this.horizontalOffset)
			return;

		this.horizontalOffset = horizontalOffset;
		painter.repaint();

		fireScrollEvent(false);
	} 

	
	
	public void scrollUpLine()
	{
		setFirstLine(getFirstLine() - 1);
	} 

	
	
	public void scrollUpPage()
	{
		setFirstLine(getFirstLine() - getVisibleLines()
			+ (lastLinePartial ? 1 : 0));
	} 

	
	
	public void scrollDownLine()
	{
		setFirstLine(getFirstLine() + 1);
	} 

	
	
	public void scrollDownPage()
	{
		setFirstLine(getFirstLine() + getVisibleLines()
			- (lastLinePartial ? 1 : 0));
	} 

	
	
	public void scrollToCaret(boolean doElectricScroll)
	{
		scrollTo(caretLine,caret - buffer.getLineStartOffset(caretLine),
			doElectricScroll);
	} 

	
	
	public void scrollTo(int offset, boolean doElectricScroll)
	{
		int line = buffer.getLineOfOffset(offset);
		scrollTo(line,offset - buffer.getLineStartOffset(line),
			doElectricScroll);
	} 

	
	
	public void scrollTo(int line, int offset, boolean doElectricScroll)
	{
		if(Debug.SCROLL_TO_DEBUG)
			Log.log(Log.DEBUG,this,"scrollTo(), lineCount="
				+ getLineCount());

		
		int extraEndVirt;
		int lineLength = buffer.getLineLength(line);
		if(offset > lineLength)
		{
			extraEndVirt = charWidth * (offset - lineLength);
			offset = lineLength;
		}
		else
			extraEndVirt = 0;

		int _electricScroll = doElectricScroll
			&& visibleLines - 1 > (electricScroll << 1)
				      ? electricScroll : 0;
		

		if(visibleLines <= 1)
		{
			if(Debug.SCROLL_TO_DEBUG)
				Log.log(Log.DEBUG,this,"visibleLines <= 0");
			setFirstPhysicalLine(line,_electricScroll);
			return;
		}

		
		int screenLine = chunkCache.getScreenLineOfOffset(line,offset);
		int visibleLines = getVisibleLines();
		if(screenLine == -1)
		{
			if(Debug.SCROLL_TO_DEBUG)
				Log.log(Log.DEBUG,this,"screenLine == -1");
			ChunkCache.LineInfo[] infos = chunkCache
				.getLineInfosForPhysicalLine(line);
			int subregion = ChunkCache.getSubregionOfOffset(
				offset,infos);
			int prevLine = displayManager.getPrevVisibleLine(getFirstPhysicalLine());
			int nextLine = displayManager.getNextVisibleLine(getLastPhysicalLine());
			if(line == getFirstPhysicalLine())
			{
				if(Debug.SCROLL_TO_DEBUG)
					Log.log(Log.DEBUG,this,line + " == " + getFirstPhysicalLine());
				setFirstPhysicalLine(line,subregion
					- _electricScroll);
			}
			else if(line == prevLine)
			{
				if(Debug.SCROLL_TO_DEBUG)
					Log.log(Log.DEBUG,this,line + " == " + prevLine);
				setFirstPhysicalLine(prevLine,subregion
					- _electricScroll);
			}
			else if(line == getLastPhysicalLine())
			{
				if(Debug.SCROLL_TO_DEBUG)
					Log.log(Log.DEBUG,this,line + " == " + getLastPhysicalLine());
				setFirstPhysicalLine(line,
					subregion + _electricScroll
					- visibleLines
					+ (lastLinePartial ? 2 : 1));
			}
			else if(line == nextLine)
			{
				if(Debug.SCROLL_TO_DEBUG)
					Log.log(Log.DEBUG,this,line + " == " + nextLine);
				setFirstPhysicalLine(nextLine,
					subregion + electricScroll
					- visibleLines
					+ (lastLinePartial ? 2 : 1));
			}
			else
			{
				if(Debug.SCROLL_TO_DEBUG)
				{
					Log.log(Log.DEBUG,this,"neither");
					Log.log(Log.DEBUG,this,"Last physical line is " + getLastPhysicalLine());
				}
				setFirstPhysicalLine(line,subregion
					- (visibleLines >> 1));
				if(Debug.SCROLL_TO_DEBUG)
				{
					Log.log(Log.DEBUG,this,"Last physical line is " + getLastPhysicalLine());
				}
			}
		}
		else if(screenLine < _electricScroll)
		{
			if(Debug.SCROLL_TO_DEBUG)
				Log.log(Log.DEBUG,this,"electric up");
			setFirstLine(getFirstLine() - _electricScroll + screenLine);
		}
		else if(screenLine > visibleLines - _electricScroll
			- (lastLinePartial ? 2 : 1))
		{
			if(Debug.SCROLL_TO_DEBUG)
				Log.log(Log.DEBUG,this,"electric down");
			setFirstLine(getFirstLine() + _electricScroll - visibleLines + screenLine + (lastLinePartial ? 2 : 1));
		} 

		
		if(!displayManager.isLineVisible(line))
			return;

		Point point = offsetToXY(line,offset,offsetXY);

		point.x += extraEndVirt;

		if(point.x < 0)
		{
			setHorizontalOffset(horizontalOffset
				- point.x + charWidth + 5);
		}
		else if(point.x >= painter.getWidth() - charWidth - 5)
		{
			setHorizontalOffset(horizontalOffset +
				(painter.getWidth() - point.x)
				- charWidth - 5);
		} 
	} 

	
	
	public final void addScrollListener(ScrollListener listener)
	{
		listenerList.add(ScrollListener.class,listener);
	} 

	
	
	public final void removeScrollListener(ScrollListener listener)
	{
		listenerList.remove(ScrollListener.class,listener);
	} 

	

	

	
	
	public int getPhysicalLineOfScreenLine(int screenLine)
	{
		return chunkCache.getLineInfo(screenLine).physicalLine;
	} 

	
	
	public int getScreenLineOfOffset(int offset)
	{
		int line = buffer.getLineOfOffset(offset);
		offset -= buffer.getLineStartOffset(line);
		return chunkCache.getScreenLineOfOffset(line,offset);
	} 

	
	
	public int getScreenLineStartOffset(int line)
	{
		ChunkCache.LineInfo lineInfo = chunkCache.getLineInfo(line);
		if(lineInfo.physicalLine == -1)
			return -1;

		return buffer.getLineStartOffset(lineInfo.physicalLine)
			+ lineInfo.offset;
	} 

	
	
	public int getScreenLineEndOffset(int line)
	{
		ChunkCache.LineInfo lineInfo = chunkCache.getLineInfo(line);
		if(lineInfo.physicalLine == -1)
			return -1;

		return buffer.getLineStartOffset(lineInfo.physicalLine)
			+ lineInfo.offset + lineInfo.length;
	} 

	

	

	
	
	public int xyToOffset(int x, int y)
	{
		return xyToOffset(x,y,true);
	} 

	
	
	public int xyToOffset(int x, int y, boolean round)
	{
		FontMetrics fm = painter.getFontMetrics();
		int height = fm.getHeight();
		int line = y / height;

		if(line < 0 || line >= visibleLines)
			return -1;

		return xToScreenLineOffset(line,x,round);
	} 

	
	
	public int xToScreenLineOffset(int screenLine, int x, boolean round)
	{
		ChunkCache.LineInfo lineInfo = chunkCache.getLineInfo(screenLine);
		if(lineInfo.physicalLine == -1)
		{
			return getLineEndOffset(displayManager
				.getLastVisibleLine()) - 1;
		}
		else
		{
			int offset = Chunk.xToOffset(lineInfo.chunks,
				x - horizontalOffset,round);
			if(offset == -1 || offset == lineInfo.offset + lineInfo.length)
				offset = lineInfo.offset + lineInfo.length - 1;

			return getLineStartOffset(lineInfo.physicalLine) + offset;
		}
	} 

	
	
	public Point offsetToXY(int offset)
	{
		int line = buffer.getLineOfOffset(offset);
		offset -= buffer.getLineStartOffset(line);
		Point retVal = new Point();
		return offsetToXY(line,offset,retVal);
	} 

	
	
	public Point offsetToXY(int line, int offset)
	{
		return offsetToXY(line,offset,new Point());
	} 

	
	
	public Point offsetToXY(int line, int offset, Point retVal)
	{
		if(!displayManager.isLineVisible(line))
			return null;
		int screenLine = chunkCache.getScreenLineOfOffset(line,offset);
		if(screenLine == -1)
			return null;

		FontMetrics fm = painter.getFontMetrics();

		retVal.y = screenLine * fm.getHeight();

		ChunkCache.LineInfo info = chunkCache.getLineInfo(screenLine);

		retVal.x = (int)(horizontalOffset + Chunk.offsetToX(
			info.chunks,offset));

		return retVal;
	} 

	

	

	
	
	public void invalidateScreenLineRange(int start, int end)
	{
		if(buffer.isLoading())
			return;

		if(start > end)
		{
			int tmp = end;
			end = start;
			start = tmp;
		}

		if(chunkCache.needFullRepaint())
			end = visibleLines;

		FontMetrics fm = painter.getFontMetrics();
		int y = start * fm.getHeight();
		int height = (end - start + 1) * fm.getHeight();
		painter.repaint(0,y,painter.getWidth(),height);
		gutter.repaint(0,y,gutter.getWidth(),height);
	} 

	
	
	public void invalidateLine(int line)
	{
		if(!isShowing()
			|| buffer.isLoading()
			|| line < getFirstPhysicalLine()
			|| line > physLastLine
			|| !displayManager.isLineVisible(line))
			return;

		int startLine = -1;
		int endLine = -1;

		for(int i = 0; i < visibleLines; i++)
		{
			ChunkCache.LineInfo info = chunkCache.getLineInfo(i);

			if((info.physicalLine >= line || info.physicalLine == -1)
				&& startLine == -1)
			{
				startLine = i;
			}

			if((info.physicalLine >= line && info.lastSubregion)
				|| info.physicalLine == -1)
			{
				endLine = i;
				break;
			}
		}

		if(chunkCache.needFullRepaint() || endLine == -1)
			endLine = visibleLines;

		invalidateScreenLineRange(startLine,endLine);
	} 

	
	
	public void invalidateLineRange(int start, int end)
	{
		if(!isShowing() || buffer.isLoading())
			return;

		if(end < start)
		{
			int tmp = end;
			end = start;
			start = tmp;
		}

		if(end < getFirstPhysicalLine() || start > getLastPhysicalLine())
			return;

		int startScreenLine = -1;
		int endScreenLine = -1;

		for(int i = 0; i < visibleLines; i++)
		{
			ChunkCache.LineInfo info = chunkCache.getLineInfo(i);

			if((info.physicalLine >= start || info.physicalLine == -1)
				&& startScreenLine == -1)
			{
				startScreenLine = i;
			}

			if((info.physicalLine >= end && info.lastSubregion)
				|| info.physicalLine == -1)
			{
				endScreenLine = i;
				break;
			}
		}

		if(startScreenLine == -1)
			startScreenLine = 0;

		if(chunkCache.needFullRepaint() || endScreenLine == -1)
			endScreenLine = visibleLines;

		invalidateScreenLineRange(startScreenLine,endScreenLine);
	} 

	

	

	
	
	public final int getBufferLength()
	{
		return buffer.getLength();
	} 

	
	
	public final int getLineCount()
	{
		return buffer.getLineCount();
	} 

	
	
	public final int getLineOfOffset(int offset)
	{
		return buffer.getLineOfOffset(offset);
	} 

	
	
	public int getLineStartOffset(int line)
	{
		return buffer.getLineStartOffset(line);
	} 

	
	
	public int getLineEndOffset(int line)
	{
		return buffer.getLineEndOffset(line);
	} 

	
	
	public int getLineLength(int line)
	{
		return buffer.getLineLength(line);
	} 

	
	
	public final String getText(int start, int len)
	{
		return buffer.getText(start,len);
	} 

	
	
	public final void getText(int start, int len, Segment segment)
	{
		buffer.getText(start,len,segment);
	} 

	
	
	public final String getLineText(int lineIndex)
	{
		return buffer.getLineText(lineIndex);
	} 

	
	
	public final void getLineText(int lineIndex, Segment segment)
	{
		buffer.getLineText(lineIndex,segment);
	} 

	
	
	public String getText()
	{
		return buffer.getText(0,buffer.getLength());
	} 

	
	
	public void setText(String text)
	{
		try
		{
			buffer.beginCompoundEdit();
			buffer.remove(0,buffer.getLength());
			buffer.insert(0,text);
		}
		finally
		{
			buffer.endCompoundEdit();
		}
	} 

	

	

	
	
	public final void selectAll()
	{
		int firstLine = getFirstLine();
		int horizOffset = getHorizontalOffset();

		setSelection(new Selection.Range(0,buffer.getLength()));
		moveCaretPosition(buffer.getLength(),true);

		setFirstLine(firstLine);
		setHorizontalOffset(horizOffset);
	} 

	
	
	public void selectLine()
	{
		int caretLine = getCaretLine();
		int start = getLineStartOffset(caretLine);
		int end = getLineEndOffset(caretLine) - 1;
		Selection s = new Selection.Range(start,end);
		if(multi)
			addToSelection(s);
		else
			setSelection(s);
		moveCaretPosition(end);
	} 

	
	
	public void selectParagraph()
	{
		int caretLine = getCaretLine();

		if(getLineLength(caretLine) == 0)
		{
			getToolkit().beep();
			return;
		}

		int start = caretLine;
		int end = caretLine;

		while(start >= 0)
		{
			if(getLineLength(start) == 0)
				break;
			else
				start--;
		}

		while(end < getLineCount())
		{
			if(getLineLength(end) == 0)
				break;
			else
				end++;
		}

		int selectionStart = getLineStartOffset(start + 1);
		int selectionEnd = getLineEndOffset(end - 1) - 1;
		Selection s = new Selection.Range(selectionStart,selectionEnd);
		if(multi)
			addToSelection(s);
		else
			setSelection(s);
		moveCaretPosition(selectionEnd);
	} 

	
	
	public void selectWord()
	{
		int line = getCaretLine();
		int lineStart = getLineStartOffset(line);
		int offset = getCaretPosition() - lineStart;

		if(getLineLength(line) == 0)
			return;

		String lineText = getLineText(line);
		String noWordSep = buffer.getStringProperty("noWordSep");

		if(offset == getLineLength(line))
			offset--;

		int wordStart = TextUtilities.findWordStart(lineText,offset,noWordSep);
		int wordEnd = TextUtilities.findWordEnd(lineText,offset+1,noWordSep);

		Selection s = new Selection.Range(lineStart + wordStart,
			lineStart + wordEnd);
		if(multi)
			addToSelection(s);
		else
			setSelection(s);
		moveCaretPosition(lineStart + wordEnd);
	} 

	
	
	public Selection selectToMatchingBracket(int position,
		boolean quickCopy)
	{
		int positionLine = buffer.getLineOfOffset(position);
		int lineOffset = position - buffer.getLineStartOffset(positionLine);
		if(getLineLength(positionLine) != 0)
		{
			int bracket = TextUtilities.findMatchingBracket(buffer,
				positionLine,Math.max(0,lineOffset - 1));

			if(bracket != -1)
			{
				Selection s;

				if(bracket < position)
				{
					if(!quickCopy)
						moveCaretPosition(position,false);
					s = new Selection.Range(bracket,position);
				}
				else
				{
					if(!quickCopy)
						moveCaretPosition(bracket + 1,false);
					s = new Selection.Range(position - 1,bracket + 1);
				}

				if(!multi && !quickCopy)
					selectNone();

				addToSelection(s);
				return s;
			}
		}

		return null;
	} 

	
	
	public void selectToMatchingBracket()
	{
		selectToMatchingBracket(caret,false);
	} 

	
	
	public void selectBlock()
	{

		Selection s = getSelectionAtOffset(caret);
		int start, end;
		if(s == null)
			start = end = caret;
		else
		{
			start = s.start;
			end = s.end;
		}

		String text = getText(0,buffer.getLength());

		
		if(start == 0)
		{
			getToolkit().beep();
			return;
		}

		
		String openBrackets = "([{";
		String closeBrackets = ")]}";
		int count = 1;
		char openBracket = '\0';
		char closeBracket = '\0';

backward_scan:	while(--start > 0)
		{
			char c = text.charAt(start);
			int index = openBrackets.indexOf(c);
			if(index != -1)
			{
				if(--count == 0)
				{
					openBracket = c;
					closeBracket = closeBrackets.charAt(index);
					break backward_scan;
				}
			}
			else if(closeBrackets.indexOf(c) != -1)
				count++;
		}

		
		count = 1;

		
		if(openBracket == '\0')
		{
			getToolkit().beep();
			return;
		}
forward_scan:	do
		{
			char c = text.charAt(end);
			if(c == closeBracket)
			{
				if(--count == 0)
				{
					end++;
					break forward_scan;
				}
			}
			else if(c == openBracket)
				count++;
		}
		while(++end < buffer.getLength());

		s = new Selection.Range(start,end);
		if(multi)
			addToSelection(s);
		else
			setSelection(s);
		moveCaretPosition(end);
	} 

	
	
	public boolean lineInStructureScope(int line)
	{
		if(match == null)
			return false;

		if(match.startLine < caretLine)
			return line >= match.startLine && line <= caretLine;
		else
			return line <= match.endLine && line >= caretLine;
	} 

	
	
	public final void invertSelection()
	{
		selectionManager.invertSelection();
	} 

	
	
	public int getSelectionCount()
	{
		return selectionManager.getSelectionCount();
	} 

	
	
	public Selection[] getSelection()
	{
		return selectionManager.getSelection();
	} 

	
	
	public Iterator<Selection> getSelectionIterator()
	{
		return selectionManager.selection.iterator();
	} 

	
	
	public Selection getSelection(int index)
	{
		return selectionManager.selection.get(index);
	} 

	
	
	public void selectNone()
	{
		invalidateSelectedLines();
		setSelection((Selection)null);
	} 

	
	
	public void setSelection(Selection[] selection)
	{
		
		invalidateSelectedLines();
		selectionManager.setSelection(selection);
		finishCaretUpdate(caretLine,NO_SCROLL,true);
	} 

	
	
	public void setSelection(Selection selection)
	{
		invalidateSelectedLines();
		selectionManager.setSelection(selection);
		finishCaretUpdate(caretLine,NO_SCROLL,true);
	} 

	
	
	public void addToSelection(Selection[] selection)
	{
		invalidateSelectedLines();
		selectionManager.addToSelection(selection);
		finishCaretUpdate(caretLine,NO_SCROLL,true);
	} 

	
	
	public void addToSelection(Selection selection)
	{
		invalidateSelectedLines();
		selectionManager.addToSelection(selection);
		finishCaretUpdate(caretLine,NO_SCROLL,true);
	} 

	
	
	public Selection getSelectionAtOffset(int offset)
	{
		return selectionManager.getSelectionAtOffset(offset);
	} 

	
	
	public void removeFromSelection(Selection sel)
	{
		invalidateSelectedLines();
		selectionManager.removeFromSelection(sel);
		finishCaretUpdate(caretLine,NO_SCROLL,true);
	} 

	
	
	public void removeFromSelection(int offset)
	{
		Selection sel = getSelectionAtOffset(offset);
		if(sel == null)
			return;

		invalidateSelectedLines();
		selectionManager.removeFromSelection(sel);
		finishCaretUpdate(caretLine,NO_SCROLL,true);
	} 

	
	
	public void resizeSelection(int offset, int end, int extraEndVirt,
		boolean rect)
	{
		Selection s = selectionManager.getSelectionAtOffset(offset);
		if(s != null)
		{
			invalidateLineRange(s.startLine,s.endLine);
			selectionManager.removeFromSelection(s);
		}

		selectionManager.resizeSelection(offset,end,extraEndVirt,rect);
		fireCaretEvent();
	} 

	
	
	public void extendSelection(int offset, int end)
	{
		extendSelection(offset,end,0,0);
	} 

	
	
	public void extendSelection(int offset, int end,
		int extraStartVirt, int extraEndVirt)
	{
		Selection s = getSelectionAtOffset(offset);
		if(s != null)
		{
			invalidateLineRange(s.startLine,s.endLine);
			selectionManager.removeFromSelection(s);

			if(offset == s.start)
			{
				offset = end;
				end = s.end;
			}
			else if(offset == s.end)
			{
				offset = s.start;
			}
		}

		if(end < offset)
		{
			int tmp = end;
			end = offset;
			offset = tmp;
		}

		if(rectangularSelectionMode)
		{
			s = new Selection.Rect(offset,end);
			((Selection.Rect)s).extraStartVirt = extraStartVirt;
			((Selection.Rect)s).extraEndVirt = extraEndVirt;
		}
		else
			s = new Selection.Range(offset,end);

		selectionManager.addToSelection(s);
		fireCaretEvent();

		if(rectangularSelectionMode && extraEndVirt != 0)
		{
			int line = getLineOfOffset(end);
			scrollTo(line,getLineLength(line) + extraEndVirt,false);
		}
	} 

	
	
	public String getSelectedText(Selection s)
	{
		StringBuffer buf = new StringBuffer(s.end - s.start);
		s.getText(buffer,buf);
		return buf.toString();
	} 

	
	
	public String getSelectedText(String separator)
	{
		Selection[] sel = selectionManager.getSelection();
		if(sel.length == 0)
			return null;

		StringBuffer buf = new StringBuffer();
		for(int i = 0; i < sel.length; i++)
		{
			if(i != 0)
				buf.append(separator);

			sel[i].getText(buffer,buf);
		}

		return buf.toString();
	} 

	
	
	public String getSelectedText()
	{
		return getSelectedText("\n");
	} 

	
	
	public void setSelectedText(Selection s, String selectedText)
	{
		if(!isEditable())
		{
			throw new InternalError("Text component"
				+ " read only");
		}

		try
		{
			buffer.beginCompoundEdit();

			moveCaretPosition(s.setText(buffer,selectedText));
		}
		
		
		finally
		{
			buffer.endCompoundEdit();
		}

		
		
	} 

	
	
	public void setSelectedText(String selectedText)
	{
		int newCaret = replaceSelection(selectedText);
		if(newCaret != -1)
			moveCaretPosition(newCaret);
		selectNone();
	} 

	
	
	public void setSelectedText(String selectedText, boolean moveCaret)
	{
		int newCaret = replaceSelection(selectedText);
		if(moveCaret && newCaret != -1)
			moveCaretPosition(newCaret);
		selectNone();
	} 

	
	
	public int replaceSelection(String selectedText)
	{
		if(!isEditable())
			throw new RuntimeException("Text component read only");

		int newCaret = -1;

		if(getSelectionCount() == 0)
		{
			
			buffer.insert(caret,selectedText);
		}
		else
		{
			try
			{

				buffer.beginCompoundEdit();

				Selection[] selection = getSelection();
				for(int i = 0; i < selection.length; i++)
					newCaret = selection[i].setText(buffer,selectedText);
			}
			finally
			{
				buffer.endCompoundEdit();
			}
		}

		return newCaret;
	} 

	
	
	public int[] getSelectedLines()
	{
		if(selectionManager.getSelectionCount() == 0)
			return new int[] { caretLine };

		return selectionManager.getSelectedLines();
	} 

	

	

	
	
	public boolean caretAutoScroll()
	{
		return focusedComponent == this;
	} 

	
	
	public void addStructureMatcher(StructureMatcher matcher)
	{
		structureMatchers.add(matcher);
	} 

	
	
	public void removeStructureMatcher(StructureMatcher matcher)
	{
		structureMatchers.remove(matcher);
	} 

	
	
	public StructureMatcher.Match getStructureMatch()
	{
		return match;
	} 

	
	
	public final void blinkCaret()
	{
		if(caretBlinks)
		{
			blink = !blink;
			invalidateLine(caretLine);
		}
		else
			blink = true;
	} 

	
	
	public void centerCaret()
	{
		int offset = getScreenLineStartOffset(visibleLines >> 1);
		if(offset == -1)
			getToolkit().beep();
		else
			setCaretPosition(offset);
	} 

	
	
	public void setCaretPosition(int newCaret)
	{
		selectNone();
		moveCaretPosition(newCaret,true);
	} 

	
	
	public void setCaretPosition(int newCaret, boolean doElectricScroll)
	{
		selectNone();
		moveCaretPosition(newCaret,doElectricScroll);
	} 

	
	
	public void moveCaretPosition(int newCaret)
	{
		moveCaretPosition(newCaret,true);
	} 

	
	
	public void moveCaretPosition(int newCaret, boolean doElectricScroll)
	{
		moveCaretPosition(newCaret,doElectricScroll ? ELECTRIC_SCROLL
			: NORMAL_SCROLL);
	} 

	
	public static final int NO_SCROLL = 0;
	public static final int NORMAL_SCROLL = 1;
	public static final int ELECTRIC_SCROLL = 2;
	
	public void moveCaretPosition(int newCaret, int scrollMode)
	{
		if(newCaret < 0 || newCaret > buffer.getLength())
		{
			throw new IllegalArgumentException("caret out of bounds: "
				+ newCaret);
		}

		int oldCaretLine = caretLine;

		if(caret == newCaret)
			finishCaretUpdate(oldCaretLine,scrollMode,false);
		else
		{
			caret = newCaret;
			caretLine = getLineOfOffset(newCaret);

			magicCaret = -1;

			finishCaretUpdate(oldCaretLine,scrollMode,true);
		}
	} 

	
	
	public int getCaretPosition()
	{
		return caret;
	} 

	
	
	public int getCaretLine()
	{
		return caretLine;
	} 

	
	
	public int getMagicCaretPosition()
	{
		if(magicCaret == -1)
		{
			magicCaret = chunkCache.subregionOffsetToX(
				caretLine,caret - getLineStartOffset(caretLine));
		}

		return magicCaret;
	} 

	
	
	public void setMagicCaretPosition(int magicCaret)
	{
		this.magicCaret = magicCaret;
	} 

	
	
	public final void addCaretListener(CaretListener listener)
	{
		listenerList.add(CaretListener.class,listener);
	} 

	
	
	public final void removeCaretListener(CaretListener listener)
	{
		listenerList.remove(CaretListener.class,listener);
	} 

	
	
	public void goToNextBracket(boolean select)
	{
		int newCaret = -1;

		if(caret != buffer.getLength())
		{
			String text = getText(caret,buffer.getLength()
				- caret - 1);

loop:			for(int i = 0; i < text.length(); i++)
			{
				switch(text.charAt(i))
				{
				case ')': case ']': case '}':
					newCaret = caret + i + 1;
					break loop;
				}
			}
		}

		if(newCaret == -1)
			getToolkit().beep();
		else
		{
			if(select)
				extendSelection(caret,newCaret);
			else if(!multi)
				selectNone();
			moveCaretPosition(newCaret);
		}
	} 

	
	
	public void goToNextCharacter(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);

		if(!select && s instanceof Selection.Range)
		{
			if(multi)
			{
				if(caret != s.end)
				{
					moveCaretPosition(s.end);
					return;
				}
			}
			else
			{
				setCaretPosition(s.end);
				return;
			}
		}

		int extraStartVirt, extraEndVirt;
		if(s instanceof Selection.Rect)
		{
			extraStartVirt = ((Selection.Rect)s).extraStartVirt;
			extraEndVirt = ((Selection.Rect)s).extraEndVirt;
		}
		else
		{
			extraStartVirt = 0;
			extraEndVirt = 0;
		}

		int newCaret = caret;

		if(caret == buffer.getLength())
		{
			if(select && (rectangularSelectionMode || s instanceof Selection.Rect))
			{
				if(s != null && caret == s.start)
					extraStartVirt++;
				else
					extraEndVirt++;
			}
			else
			{
				getToolkit().beep();
				return;
			}
		}
		else if(caret == getLineEndOffset(caretLine) - 1)
		{
			if(select && (rectangularSelectionMode || s instanceof Selection.Rect))
			{
				if(s != null && caret == s.start)
					extraStartVirt++;
				else
					extraEndVirt++;
			}
			else
			{
				int line = displayManager.getNextVisibleLine(caretLine);
				if(line == -1)
				{
					getToolkit().beep();
					return;
				}
				else
					newCaret = getLineStartOffset(line);
			}
		}
		else
			newCaret = caret + 1;

		if(select)
			extendSelection(caret,newCaret,extraStartVirt,extraEndVirt);
		else if(!multi)
			selectNone();

		moveCaretPosition(newCaret);
	} 

	
	
	public void goToNextLine(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);
		boolean rectSelect = s == null ? rectangularSelectionMode
			: s instanceof Selection.Rect;
		int magic = getMagicCaretPosition();
		int newCaret = chunkCache.getBelowPosition(caretLine,
			caret - buffer.getLineStartOffset(caretLine),magic + 1,
			rectSelect && select);
		if(newCaret == -1)
		{
			int end = getLineEndOffset(caretLine) - 1;
			if(caret == end)
			{
				getToolkit().beep();
				return;
			}
			else
				newCaret = end;
		}

		_changeLine(select, newCaret);

		setMagicCaretPosition(magic);
	}

	
	
	public void goToNextPage(boolean select)
	{
		scrollToCaret(false);
		int magic = getMagicCaretPosition();
		if(caretLine < displayManager.getFirstVisibleLine())
		{
			caretLine = displayManager.getNextVisibleLine(
				caretLine);
		}

		int newCaret;

		if(getFirstLine() + getVisibleLines() >= displayManager
			.getScrollLineCount())
		{
			int lastVisibleLine = displayManager
				.getLastVisibleLine();
			newCaret = getLineEndOffset(lastVisibleLine) - 1;
		}
		else
		{
			int caretScreenLine = getScreenLineOfOffset(caret);

			scrollDownPage();

			newCaret = xToScreenLineOffset(caretScreenLine,
				magic,true);
		}

		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();

		moveCaretPosition(newCaret,false);

		setMagicCaretPosition(magic);
	} 

	
	
	public void goToNextParagraph(boolean select)
	{
		int lineNo = getCaretLine();

		int newCaret = getBufferLength();

		boolean foundBlank = false;

loop:		for(int i = lineNo + 1; i < getLineCount(); i++)
		{
			if(!displayManager.isLineVisible(i))
				continue;

			getLineText(i,lineSegment);

			for(int j = 0; j < lineSegment.count; j++)
			{
				switch(lineSegment.array[lineSegment.offset + j])
				{
				case ' ':
				case '\t':
					break;
				default:
					if(foundBlank)
					{
						newCaret = getLineStartOffset(i);
						break loop;
					}
					else
						continue loop;
				}
			}

			foundBlank = true;
		}

		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();
		moveCaretPosition(newCaret);
	} 

	
	
	public void goToNextWord(boolean select)
	{
		goToNextWord(select,false);
	} 

	
	
	public void goToNextWord(boolean select, boolean eatWhitespace)
	{
		int lineStart = getLineStartOffset(caretLine);
		int newCaret = caret - lineStart;
		String lineText = getLineText(caretLine);

		if(newCaret == lineText.length())
		{
			int nextLine = displayManager.getNextVisibleLine(caretLine);
			if(nextLine == -1)
			{
				getToolkit().beep();
				return;
			}

			newCaret = getLineStartOffset(nextLine);
		}
		else
		{
			String noWordSep = buffer.getStringProperty("noWordSep");
			newCaret = TextUtilities.findWordEnd(lineText,
				newCaret + 1,noWordSep,true,eatWhitespace);

			newCaret += lineStart;
		}

		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();
		moveCaretPosition(newCaret);
	} 

	
	
	public void goToPrevBracket(boolean select)
	{
		String text = getText(0,caret);

		int newCaret = -1;

loop:		for(int i = getCaretPosition() - 1; i >= 0; i--)
		{
			switch(text.charAt(i))
			{
			case '(': case '[': case '{':
				newCaret = i;
				break loop;
			}
		}

		if(newCaret == -1)
			getToolkit().beep();
		else
		{
			if(select)
				extendSelection(caret,newCaret);
			else if(!multi)
				selectNone();
			moveCaretPosition(newCaret);
		}
	} 

	
	
	public void goToPrevCharacter(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);

		if(caret == 0)
		{
			getToolkit().beep();
			return;
		}

		if(!select && s instanceof Selection.Range)
		{
			if(multi)
			{
				if(caret != s.start)
				{
					moveCaretPosition(s.start);
					return;
				}
			}
			else
			{
				setCaretPosition(s.start);
				return;
			}
		}

		int extraStartVirt = 0;
		int extraEndVirt = 0;
		int newCaret = caret;

		if(select && caret == getLineEndOffset(caretLine) - 1)
		{
			if(s instanceof Selection.Rect)
			{
				extraStartVirt = ((Selection.Rect)s).extraStartVirt;
				extraEndVirt = ((Selection.Rect)s).extraEndVirt;
				if(caret == s.start)
				{
					if(extraStartVirt == 0)
						newCaret = caret - 1;
					else
						extraStartVirt--;
				}
				else
				{
					if(extraEndVirt == 0)
						newCaret = caret - 1;
					else
						extraEndVirt--;
				}
			}
			else
				newCaret = caret - 1;
		}
		else if(caret == getLineStartOffset(caretLine))
		{
			int line = displayManager.getPrevVisibleLine(caretLine);
			if(line == -1)
			{
				getToolkit().beep();
				return;
			}
			newCaret = getLineEndOffset(line) - 1;
		}
		else
			newCaret = caret - 1;

		if(select)
			extendSelection(caret,newCaret,extraStartVirt,extraEndVirt);
		else if(!multi)
			selectNone();
		moveCaretPosition(newCaret);
	} 

	
	
	public void goToPrevLine(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);
		boolean rectSelect = s == null ? rectangularSelectionMode
			: s instanceof Selection.Rect;
		int magic = getMagicCaretPosition();

		int newCaret = chunkCache.getAbovePosition(caretLine,
			caret - buffer.getLineStartOffset(caretLine),magic + 1,
			rectSelect && select);
		if(newCaret == -1)
		{
			int start = getLineStartOffset(caretLine);
			if(caret == start)
			{
				getToolkit().beep();
				return;
			}
			else
				newCaret = start;
		}

		_changeLine(select, newCaret);

		setMagicCaretPosition(magic);
	} 

	
	
	public void goToPrevPage(boolean select)
	{
		scrollToCaret(false);
		int magic = getMagicCaretPosition();

		if(caretLine < displayManager.getFirstVisibleLine())
		{
			caretLine = displayManager.getNextVisibleLine(
				caretLine);
		}

		int newCaret;

		if(getFirstLine() == 0)
		{
			int firstVisibleLine = displayManager
				.getFirstVisibleLine();
			newCaret = getLineStartOffset(firstVisibleLine);
		}
		else
		{
			int caretScreenLine = getScreenLineOfOffset(caret);

			scrollUpPage();

			newCaret = xToScreenLineOffset(caretScreenLine,
				magic,true);
		}

		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();
		moveCaretPosition(newCaret,false);

		setMagicCaretPosition(magic);
	} 

	
	
	public void goToPrevParagraph(boolean select)
	{
		int lineNo = caretLine;
		int newCaret = 0;

		boolean foundBlank = false;

loop:		for(int i = lineNo - 1; i >= 0; i--)
		{
			if(!displayManager.isLineVisible(i))
				continue;

			getLineText(i,lineSegment);

			for(int j = 0; j < lineSegment.count; j++)
			{
				switch(lineSegment.array[lineSegment.offset + j])
				{
				case ' ':
				case '\t':
					break;
				default:
					if(foundBlank)
					{
						newCaret = getLineEndOffset(i) - 1;
						break loop;
					}
					else
						continue loop;
				}
			}

			foundBlank = true;
		}

		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();
		moveCaretPosition(newCaret);
	} 

	
	
	public void goToPrevWord(boolean select)
	{
		goToPrevWord(select,false);
	} 

	
	
	public void goToPrevWord(boolean select, boolean eatWhitespace)
	{
		int lineStart = getLineStartOffset(caretLine);
		int newCaret = caret - lineStart;
		String lineText = getLineText(caretLine);

		if(newCaret == 0)
		{
			if(lineStart == 0)
			{
				getToolkit().beep();
				return;
			}
			else
			{
				int prevLine = displayManager.getPrevVisibleLine(caretLine);
				if(prevLine == -1)
				{
					getToolkit().beep();
					return;
				}

				newCaret = getLineEndOffset(prevLine) - 1;
			}
		}
		else
		{
			String noWordSep = buffer.getStringProperty("noWordSep");
			newCaret = TextUtilities.findWordStart(lineText,
				newCaret - 1,noWordSep,true,eatWhitespace);

			newCaret += lineStart;
		}

		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();
		moveCaretPosition(newCaret);
	} 

	
	
	public void smartHome(boolean select)
	{
		switch(getInputHandler().getLastActionCount())
		{
		case 1:
			goToStartOfWhiteSpace(select);
			break;
		case 2:
			goToStartOfLine(select);
			break;
		default: 
			goToFirstVisibleLine(select);
			break;
		}
	} 

	
	
	public void smartEnd(boolean select)
	{
		switch(getInputHandler().getLastActionCount())
		{
		case 1:
			goToEndOfWhiteSpace(select);
			break;
		case 2:
			goToEndOfLine(select);
			break;
		default: 
			goToLastVisibleLine(select);
			break;
		}
	} 

	
	
	public void goToStartOfLine(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);
		int line = select || s == null ? caretLine : s.startLine;
		int newCaret = getLineStartOffset(line);
		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();
		moveCaretPosition(newCaret);
	} 

	
	
	public void goToEndOfLine(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);
		int line = select || s == null ? caretLine : s.endLine;
		int newCaret = getLineEndOffset(line) - 1;
		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();
		moveCaretPosition(newCaret);

		
		
		
	} 

	
	
	public void goToStartOfWhiteSpace(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);
		int line, offset;
		if(select || s == null)
		{
			line = caretLine;
			offset = caret - buffer.getLineStartOffset(line);
		}
		else
		{
			line = s.startLine;
			offset = s.start - buffer.getLineStartOffset(line);
		}

		int firstIndent = chunkCache.getSubregionStartOffset(line,offset);
		if(firstIndent == getLineStartOffset(line))
		{
			firstIndent = StandardUtilities.getLeadingWhiteSpace(getLineText(line));
			if(firstIndent == getLineLength(line))
				firstIndent = 0;
			firstIndent += getLineStartOffset(line);
		}

		if(select)
			extendSelection(caret,firstIndent);
		else if(!multi)
			selectNone();
		moveCaretPosition(firstIndent);
	} 

	
	
	public void goToEndOfWhiteSpace(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);
		int line, offset;
		if(select || s == null)
		{
			line = caretLine;
			offset = caret - getLineStartOffset(line);
		}
		else
		{
			line = s.endLine;
			offset = s.end - getLineStartOffset(line);
		}

		int lastIndent = chunkCache.getSubregionEndOffset(line,offset);

		if(lastIndent == getLineEndOffset(line))
		{
			lastIndent = getLineLength(line) - StandardUtilities.getTrailingWhiteSpace(getLineText(line));
			if(lastIndent == 0)
				lastIndent = getLineLength(line);
			lastIndent += getLineStartOffset(line);
		}
		else
		{
			lastIndent--;
		}

		if(select)
			extendSelection(caret,lastIndent);
		else if(!multi)
			selectNone();
		moveCaretPosition(lastIndent);
	} 

	
	
	public void goToFirstVisibleLine(boolean select)
	{
		int firstVisibleLine = getFirstLine() == 0 ? 0 : electricScroll;
		int firstVisible = getScreenLineStartOffset(firstVisibleLine);
		if(firstVisible == -1)
		{
			firstVisible = getLineStartOffset(displayManager
				.getFirstVisibleLine());
		}

		if(select)
			extendSelection(caret,firstVisible);
		else if(!multi)
			selectNone();
		moveCaretPosition(firstVisible);
	} 

	
	
	public void goToLastVisibleLine(boolean select)
	{
		int lastVisible;

		if(getFirstLine() + visibleLines >=
			displayManager.getScrollLineCount())
		{
			lastVisible = getLineEndOffset(displayManager
				.getLastVisibleLine()) - 1;
		}
		else
		{
			lastVisible = visibleLines - electricScroll - 1;
			if(lastLinePartial)
				lastVisible--;
			if(lastVisible < 0)
				lastVisible = 0;
			lastVisible = getScreenLineEndOffset(lastVisible) - 1;
			if(lastVisible == -1)
			{
				lastVisible = getLineEndOffset(displayManager
					.getLastVisibleLine()) - 1;
			}
		}

		if(select)
			extendSelection(caret,lastVisible);
		else if(!multi)
			selectNone();
		moveCaretPosition(lastVisible);
	} 

	
	
	public void goToBufferStart(boolean select)
	{
		int start = buffer.getLineStartOffset(
			displayManager.getFirstVisibleLine());
		if(select)
			extendSelection(caret,start);
		else if(!multi)
			selectNone();
		moveCaretPosition(start);
	} 

	
	
	public void goToBufferEnd(boolean select)
	{
		int end = buffer.getLineEndOffset(
			displayManager.getLastVisibleLine()) - 1;
		if(select)
			extendSelection(caret,end);
		else if(!multi)
			selectNone();
		moveCaretPosition(end);
	} 

	
	
	public void goToMatchingBracket()
	{
		if(getLineLength(caretLine) != 0)
		{
			int dot = caret - getLineStartOffset(caretLine);

			int bracket = TextUtilities.findMatchingBracket(
				buffer,caretLine,Math.max(0,dot - 1));
			if(bracket != -1)
			{
				selectNone();
				moveCaretPosition(bracket + 1,false);
				return;
			}
		}

		getToolkit().beep();
	} 

	

	

	
	
	public void userInput(char ch)
	{
		if(!isEditable())
		{
			getToolkit().beep();
			return;
		}

		
		if(hiddenCursor != null)
			getPainter().setCursor(hiddenCursor);

		if(ch == '\t')
			userInputTab();
		else
		{
			boolean indent = buffer.isElectricKey(ch, caretLine);
			String str = String.valueOf(ch);
			if(getSelectionCount() == 0)
			{
				if(!doWordWrap(ch == ' '))
					insert(str,indent);
			}
			else
				replaceSelection(str);
		}
	} 

	
	
	public final boolean isOverwriteEnabled()
	{
		return overwrite;
	} 

	
	
	public final void setOverwriteEnabled(boolean overwrite)
	{
		blink = true;
		caretTimer.restart();

		this.overwrite = overwrite;
		invalidateLine(caretLine);
		fireStatusChanged(StatusListener.OVERWRITE_CHANGED,overwrite);
	} 

	
	
	public final void toggleOverwriteEnabled()
	{
		setOverwriteEnabled(!overwrite);
	} 

	
	
	public void backspace()
	{
		delete(false);
	} 

	
	
	public void backspaceWord()
	{
		backspaceWord(false);
	} 

	
	
	public void backspaceWord(boolean eatWhitespace)
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(getSelectionCount() != 0)
		{
			setSelectedText("");
			return;
		}

		int lineStart = getLineStartOffset(caretLine);
		int _caret = caret - lineStart;

		String lineText = getLineText(caretLine);

		if(_caret == 0)
		{
			if(lineStart == 0)
			{
				getToolkit().beep();
				return;
			}
			_caret--;
		}
		else
		{
			String noWordSep = buffer.getStringProperty("noWordSep");
			_caret = TextUtilities.findWordStart(lineText,_caret-1,
				noWordSep,true,eatWhitespace);
		}

		buffer.remove(_caret + lineStart,
			caret - (_caret + lineStart));
	} 

	
	
	public void delete()
	{
		delete(true);
	} 

	
	
	public void deleteToEndOfLine()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		buffer.remove(caret,getLineEndOffset(caretLine)
			- caret - 1);
	} 

	
	
	public void deleteLine()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		int x = chunkCache.subregionOffsetToX(caretLine,caret - getLineStartOffset(caretLine));
		int[] lines = getSelectedLines();

		try
		{
			buffer.beginCompoundEdit();

			for (int i = lines.length - 1; i >= 0; i--)
			{
				int start = getLineStartOffset(lines[i]);
				int end = getLineEndOffset(lines[i]);
				if (end > buffer.getLength())
				{
					if (start != 0)
						start--;
					end--;
				}
				buffer.remove(start,end - start);
			}
		}
		finally
		{
			buffer.endCompoundEdit();
		}

		int lastLine = displayManager.getLastVisibleLine();

		if(caretLine == lastLine)
		{
			int offset = chunkCache.xToSubregionOffset(lastLine,0,x,true);
			setCaretPosition(buffer.getLineStartOffset(lastLine)
			+ offset);
		}
		else
		{
			int offset = chunkCache.xToSubregionOffset(caretLine,0,x,true);
			setCaretPosition(getLineStartOffset(caretLine) + offset);
		}
	} 

	
	
	public void deleteParagraph()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		
		int start = 0;
		for(int i = caretLine - 1; i >= 0; i--)
		{
			if (lineContainsSpaceAndTabs(i))
			{
				start = getLineStartOffset(i);
				break;
			}
		}

		
		int end = buffer.getLength();
		for(int i = caretLine + 1; i < getLineCount(); i++)
		{
			
			

			if (lineContainsSpaceAndTabs(i))
			{
				end = getLineEndOffset(i) - 1;
				break;
			}
		}

		buffer.remove(start,end - start);
	} 

	
	
	public void deleteToStartOfLine()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		buffer.remove(getLineStartOffset(caretLine),
			caret - getLineStartOffset(caretLine));
	} 

	
	
	public void deleteWord()
	{
		deleteWord(false);
	} 

	
	
	public void deleteWord(boolean eatWhitespace)
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(getSelectionCount() != 0)
		{
			setSelectedText("");
			return;
		}

		int lineStart = getLineStartOffset(caretLine);
		int _caret = caret - lineStart;

		String lineText = getLineText(caretLine);

		if(_caret == lineText.length())
		{
			if(lineStart + _caret == buffer.getLength())
			{
				getToolkit().beep();
				return;
			}
			_caret++;
		}
		else
		{
			String noWordSep = buffer.getStringProperty("noWordSep");
			_caret = TextUtilities.findWordEnd(lineText,
				_caret+1,noWordSep,true,eatWhitespace);
		}

		buffer.remove(caret,(_caret + lineStart) - caret);
	} 

	
	
	public final boolean isMultipleSelectionEnabled()
	{
		return multi;
	} 

	
	
	public final void toggleMultipleSelectionEnabled()
	{
		setMultipleSelectionEnabled(!multi);
	} 

	
	
	public final void setMultipleSelectionEnabled(boolean multi)
	{
		this.multi = multi;
		fireStatusChanged(StatusListener.MULTI_SELECT_CHANGED,multi);
		painter.repaint();
	} 

	
	
	public final boolean isRectangularSelectionEnabled()
	{
		return rectangularSelectionMode;
	} 

	
	
	public final void toggleRectangularSelectionEnabled()
	{
		setRectangularSelectionEnabled(!rectangularSelectionMode);

		if(getSelectionCount() == 1)
		{
			Selection s = getSelection(0);
			removeFromSelection(s);
			if(rectangularSelectionMode)
			{
				addToSelection(new Selection.Rect(
					s.getStart(),s.getEnd()));
			}
			else
			{
				addToSelection(new Selection.Range(
					s.getStart(),s.getEnd()));
			}
		}
	} 

	
	
	public final void setRectangularSelectionEnabled(
		boolean rectangularSelectionMode)
	{
		this.rectangularSelectionMode = rectangularSelectionMode;
		fireStatusChanged(StatusListener.RECT_SELECT_CHANGED,
			rectangularSelectionMode);
		painter.repaint();
	} 

	

	

	
	
	public void goToParentFold()
	{
		int line = -1;
		int level = buffer.getFoldLevel(caretLine);
		for(int i = caretLine - 1; i >= 0; i--)
		{
			if(buffer.getFoldLevel(i) < level)
			{
				line = i;
				break;
			}
		}

		if(line == -1)
		{
			getToolkit().beep();
			return;
		}

		int magic = getMagicCaretPosition();

		int newCaret = buffer.getLineStartOffset(line)
			+ chunkCache.xToSubregionOffset(line,0,magic + 1,true);
		if(!multi)
			selectNone();

		moveCaretPosition(newCaret);
		setMagicCaretPosition(magic);
	} 

	
	
	public void goToNextFold(boolean select)
	{
		int nextFold = -1;
		for(int i = caretLine + 1; i < buffer.getLineCount(); i++)
		{
			if(buffer.isFoldStart(i)
				&& displayManager.isLineVisible(i))
			{
				nextFold = i;
				break;
			}
		}

		if(nextFold == -1)
		{
			getToolkit().beep();
			return;
		}

		int magic = getMagicCaretPosition();

		int newCaret = buffer.getLineStartOffset(nextFold)
			+ chunkCache.xToSubregionOffset(nextFold,0,magic + 1,true);
		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();

		moveCaretPosition(newCaret);
		setMagicCaretPosition(magic);
	} 

	
	
	public void goToPrevFold(boolean select)
	{
		int prevFold = -1;
		for(int i = caretLine - 1; i >= 0; i--)
		{
			if(buffer.isFoldStart(i)
				&& displayManager.isLineVisible(i))
			{
				prevFold = i;
				break;
			}
		}

		if(prevFold == -1)
		{
			getToolkit().beep();
			return;
		}

		int magic = getMagicCaretPosition();

		int newCaret = buffer.getLineStartOffset(prevFold)
			+ chunkCache.xToSubregionOffset(prevFold,0,magic + 1,true);
		if(select)
			extendSelection(caret,newCaret);
		else if(!multi)
			selectNone();

		moveCaretPosition(newCaret);
		setMagicCaretPosition(magic);
	} 

	
	
	public void collapseFold()
	{
		collapseFold(caretLine);
	} 

	
	
	public void collapseFold(int line)
	{
		int x = chunkCache.subregionOffsetToX(caretLine,
			caret - getLineStartOffset(caretLine));

		displayManager.collapseFold(line);

		if(displayManager.isLineVisible(caretLine))
			return;

		line = displayManager.getPrevVisibleLine(caretLine);

		if(!multi)
		{
			
			invalidateSelectedLines();
			selectionManager.setSelection((Selection) null);
		}
		moveCaretPosition(buffer.getLineStartOffset(line)
			+ chunkCache.xToSubregionOffset(line,0,x,true));
	} 

	
	
	public void expandFold(boolean fully)
	{
		int x = chunkCache.subregionOffsetToX(caretLine,
			caret - getLineStartOffset(caretLine));

		int line = displayManager.expandFold(caretLine,fully);

		if(!fully && line != -1)
		{
			if(!multi)
				selectNone();
			moveCaretPosition(getLineStartOffset(line)
				+ chunkCache.xToSubregionOffset(line,0,x,true));
		}
	} 

	
	
	public void selectFold()
	{
		selectFold(caretLine);
	} 

	
	
	public void selectFold(int line)
	{
		int[] lines = buffer.getFoldAtLine(line);

		int newCaret = getLineEndOffset(lines[1]) - 1;
		Selection s = new Selection.Range(getLineStartOffset(lines[0]),newCaret);
		if(multi)
			addToSelection(s);
		else
			setSelection(s);
		moveCaretPosition(newCaret);
	} 

	
	
	public void narrowToFold()
	{
		int[] lines = buffer.getFoldAtLine(caretLine);
		if(lines[0] == 0 && lines[1] == buffer.getLineCount() - 1)
			getToolkit().beep();
		else
			displayManager.narrow(lines[0],lines[1]);
	} 

	
	
	public void narrowToSelection()
	{
		if(getSelectionCount() != 1)
		{
			getToolkit().beep();
			return;
		}

		Selection sel = getSelection(0);
		displayManager.narrow(sel.getStartLine(),sel.getEndLine());

		selectNone();
	} 

	
	
	public void addExplicitFold() throws TextAreaException
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}
		if(!buffer.getStringProperty("folding").equals("explicit"))
		{
			throw new TextAreaException("folding-not-explicit");
		}

		try
		{
			buffer.beginCompoundEdit();

			if (getSelectionCount() == 0)
			{
				addExplicitFold(caret, caret, caretLine, caretLine);
			}
			else
			{
				Selection[] selections = getSelection();
				Selection selection = null;
				int caretBack = 0;
				for (int i = 0; i < selections.length; i++)
				{
					selection = selections[i];
					caretBack = addExplicitFold(selection.start, selection.end, selection.startLine,selection.endLine);
				}
				
				assert selection != null;
				setCaretPosition(selection.start - caretBack, false);
			}
		}
		finally
		{
			buffer.endCompoundEdit();
		}
	} 
	

	

	
	
	public void lineComment()
	{
		String comment = buffer.getContextSensitiveProperty(caret,"lineComment");
		if(!buffer.isEditable() || comment == null || comment.length() == 0)
		{
			getToolkit().beep();
			return;
		}

		comment += ' ';

		buffer.beginCompoundEdit();

		int[] lines = getSelectedLines();

		try
		{
			for(int i = 0; i < lines.length; i++)
			{
				String text = getLineText(lines[i]);
				buffer.insert(getLineStartOffset(lines[i])
					+ StandardUtilities.getLeadingWhiteSpace(text),
					comment);
			}
		}
		finally
		{
			buffer.endCompoundEdit();
		}

		selectNone();
	} 

	
	
	public void rangeComment()
	{
		String commentStart = buffer.getContextSensitiveProperty(caret,"commentStart");
		String commentEnd = buffer.getContextSensitiveProperty(caret,"commentEnd");
		if(!buffer.isEditable() || commentStart == null || commentEnd == null
			|| commentStart.length() == 0 || commentEnd.length() == 0)
		{
			getToolkit().beep();
			return;
		}

		commentStart += ' ';
		commentEnd = ' ' + commentEnd;

		try
		{
			buffer.beginCompoundEdit();

			Selection[] selection = getSelection();

			if(selection.length == 0)
			{
				int oldCaret = caret;
				buffer.insert(caret,commentStart);
				buffer.insert(caret,commentEnd);
				setCaretPosition(oldCaret + commentStart.length());
			}

			for(int i = 0; i < selection.length; i++)
			{
				Selection s = selection[i];
				if(s instanceof Selection.Range)
				{
					buffer.insert(s.start,commentStart);
					buffer.insert(s.end,commentEnd);
				}
				else if(s instanceof Selection.Rect)
				{
					Selection.Rect rect = (Selection.Rect)s;
					int start = rect.getStartColumn(buffer);
					int end = rect.getEndColumn(buffer);

					for(int j = s.startLine; j <= s.endLine; j++)
					{
						buffer.insertAtColumn(j,end,
							commentEnd);
						buffer.insertAtColumn(j,start,
							commentStart);
					}
				}
			}

			selectNone();
		}
		finally
		{
			buffer.endCompoundEdit();
		}
	} 

	
	
	public void formatParagraph() throws TextAreaException
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(maxLineLen <= 0)
		{
			throw new TextAreaException("format-maxlinelen");
		}

		Selection[] selection = getSelection();
		if(selection.length != 0)
		{
			buffer.beginCompoundEdit();

			for(int i = 0; i < selection.length; i++)
			{
				Selection s = selection[i];
				setSelectedText(s,TextUtilities.format(
					getSelectedText(s),maxLineLen,
					buffer.getTabSize()));
			}

			buffer.endCompoundEdit();
		}
		else
		{
			int lineNo = getCaretLine();

			int start = 0, end = buffer.getLength();

			for(int i = lineNo - 1; i >= 0; i--)
			{
				if (lineContainsSpaceAndTabs(i))
				{
					start = getLineEndOffset(i);
					break;
				}
			}

			for(int i = lineNo + 1; i < getLineCount(); i++)
			{
				if (lineContainsSpaceAndTabs(i))
				{
					end = getLineStartOffset(i) - 1;
					break;
				}
			}

			try
			{
				buffer.beginCompoundEdit();

				String text = buffer.getText(start,end - start);
				int offset = getCaretPosition() - start;
				int noSpaceOffset = TextUtilities.indexIgnoringWhitespace(
					text,offset);
				buffer.remove(start,end - start);
				text = TextUtilities.format(
					text,maxLineLen,buffer.getTabSize());
				buffer.insert(start,text);
				int caretPos = start;
				if (text.length() != 0)
				{
					caretPos += Math.min(text.length(),
					TextUtilities.ignoringWhitespaceIndex(
					text,noSpaceOffset));
				}
				moveCaretPosition(caretPos);
			}
			finally
			{
				buffer.endCompoundEdit();
			}
		}
	} 

	
	
	public void spacesToTabs()
	{
		Selection[] selection = getSelection();

		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		buffer.beginCompoundEdit();

		if(selection.length == 0)
		{
			setText(TextUtilities.spacesToTabs(
				getText(), buffer.getTabSize()));
		}
		else
		{
			for(int i = 0; i < selection.length; i++)
			{
				Selection s = selection[i];
				setSelectedText(s,TextUtilities.spacesToTabs(
					getSelectedText(s),buffer.getTabSize()));
			}
		}

		buffer.endCompoundEdit();
	} 

	
	
	public void tabsToSpaces()
	{
		Selection[] selection = getSelection();

		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		buffer.beginCompoundEdit();

		if(selection.length == 0)
		{
			setText(TextUtilities.tabsToSpaces(
				getText(), buffer.getTabSize()));
		}
		else
		{
			for(int i = 0; i < selection.length; i++)
			{
				Selection s = selection[i];
				setSelectedText(s,TextUtilities.tabsToSpaces(
					getSelectedText(s),buffer.getTabSize()));
			}
		}

		buffer.endCompoundEdit();
	} 

	
	
	public void toUpperCase()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		Selection[] selection = getSelection();
		int caret = -1;
		if (selection.length == 0)
		{
			caret = getCaretPosition();
			selectWord();
			selection = getSelection();
		}
		if (selection.length == 0)
		{
			if (caret != -1)
				setCaretPosition(caret);
			getToolkit().beep();
			return;
		}

		buffer.beginCompoundEdit();

		for(int i = 0; i < selection.length; i++)
		{
			Selection s = selection[i];
			setSelectedText(s,getSelectedText(s).toUpperCase());
		}

		buffer.endCompoundEdit();
		if (caret != -1)
			setCaretPosition(caret);
	} 

	
	
	public void toLowerCase()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		Selection[] selection = getSelection();
		int caret = -1;
		if (selection.length == 0)
		{
			caret = getCaretPosition();
			selectWord();
			selection = getSelection();
		}
		if (selection.length == 0)
		{
			if (caret != -1)
				setCaretPosition(caret);
			getToolkit().beep();
			return;
		}

		buffer.beginCompoundEdit();

		for (int i = 0; i < selection.length; i++)
		{
			Selection s = selection[i];
			setSelectedText(s,getSelectedText(s).toLowerCase());
		}

		buffer.endCompoundEdit();
		if (caret != -1)
			setCaretPosition(caret);
	} 

	
	
	public void removeTrailingWhiteSpace()
	{
		if(!buffer.isEditable())
			getToolkit().beep();
		else
		{
			buffer.removeTrailingWhiteSpace(getSelectedLines());
		}
	} 

	
	public void insertEnterAndIndent()
	{
		if(!isEditable())
			getToolkit().beep();
		else
		{
			try
			{
				buffer.beginCompoundEdit();
				setSelectedText("\n");
				buffer.indentLine(caretLine,true);
			}
			finally
			{
				buffer.endCompoundEdit();
			}
		}
	} 

	
	public void insertTabAndIndent()
	{
		if(!isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(getSelectionCount() == 0)
		{
			
			String text = buffer.getLineText(caretLine);
			int start = buffer.getLineStartOffset(caretLine);
			int whiteSpace = StandardUtilities.getLeadingWhiteSpace(text);

			if(caret - start <= whiteSpace
				&& buffer.indentLine(caretLine,false))
				return;
		}

		userInput('\t');
	} 

	
	
	public void indentSelectedLines()
	{
		if(!buffer.isEditable())
			getToolkit().beep();
		else
		{
			buffer.indentLines(getSelectedLines());
			selectNone();
		}
	} 

	
	
	public void shiftIndentLeft()
	{
		if(!buffer.isEditable())
			getToolkit().beep();
		else
		{
			buffer.shiftIndentLeft(getSelectedLines());
		}
	} 

	
	
	public void shiftIndentRight()
	{
		if(!buffer.isEditable())
			getToolkit().beep();
		else
			buffer.shiftIndentRight(getSelectedLines());
	} 

	
	
	public void joinLines()
	{
		if (getSelectionCount() == 0)
		{
			int end = getLineEndOffset(caretLine);
			if(!buffer.isEditable() || end > buffer.getLength())
			{
				getToolkit().beep();
				return;
			}

			try
			{
				buffer.beginCompoundEdit();
				String nextLineText = buffer.getLineText(caretLine + 1);
				buffer.remove(end - 1,StandardUtilities.getLeadingWhiteSpace(
					nextLineText) + 1);
				if (nextLineText.length() != 0)
					buffer.insert(end - 1, " ");
			}
			finally
			{
				buffer.endCompoundEdit();
			}
			setCaretPosition(end - 1);
		}
		else
		{
			try
			{
				buffer.beginCompoundEdit();
				Selection[] selections = selectionManager.getSelection();
				for (int i = 0; i < selections.length; i++)
				{
					Selection selection = selections[i];
					joinLines(selection);
				}
			}
			finally
			{
				buffer.endCompoundEdit();
			}
		}
	} 

	
	
	private void joinLines(Selection selection)
	{
		do
		{
			if (selection.startLine == buffer.getLineCount() - 1)
				return;
			int end = getLineEndOffset(selection.startLine);
			String nextLineText = buffer.getLineText(selection.startLine + 1);
			buffer.remove(end - 1,StandardUtilities.getLeadingWhiteSpace(
				nextLineText) + 1);
			if (nextLineText.length() != 0)
				buffer.insert(end - 1, " ");
		}
		while (selection.startLine < selection.endLine);
	} 
	

	

	
	
	public void addLeftOfScrollBar(Component comp)
	{
		verticalBox.add(comp,verticalBox.getComponentCount() - 1);
	} 

	
	
	public void removeLeftOfScrollBar(Component comp)
	{
		verticalBox.remove(comp);
	} 

	
	
	public void addNotify()
	{
		super.addNotify();

		ToolTipManager.sharedInstance().registerComponent(painter);
		ToolTipManager.sharedInstance().registerComponent(gutter);

		recalculateVisibleLines();
		if(!buffer.isLoading())
			recalculateLastPhysicalLine();
		propertiesChanged();

		hiddenCursor = getToolkit().createCustomCursor(
			getGraphicsConfiguration()
			.createCompatibleImage(16,16,
			Transparency.BITMASK),
			new Point(0,0),"Hidden");
	} 

	
	
	public void removeNotify()
	{
		super.removeNotify();

		ToolTipManager.sharedInstance().unregisterComponent(painter);
		ToolTipManager.sharedInstance().unregisterComponent(gutter);

		if(focusedComponent == this)
			focusedComponent = null;
	} 

	
	
	public boolean getFocusTraversalKeysEnabled()
	{
		return false;
	} 

	
	
	public boolean getFocusCycleRoot()
	{
		return true;
	} 

	
	public void processKeyEvent(KeyEvent evt)
	{
		getInputHandler().processKeyEvent(evt,1, false);
		if(!evt.isConsumed())
			super.processKeyEvent(evt);

	} 

	
	
	public void addTopComponent(Component comp)
	{
		add(ScrollLayout.TOP,comp);
	} 

	
	
	public void removeTopComponent(Component comp)
	{
		remove(comp);
	} 

	
	private InputMethodSupport inputMethodSupport;
	public InputMethodRequests getInputMethodRequests()
	{
		if(inputMethodSupport == null)
		{
			inputMethodSupport = new InputMethodSupport(this);
			Log.log(Log.DEBUG, this, "InputMethodSupport is activated");
		}
		return inputMethodSupport;
	} 
	

	
	
	public final void addStatusListener(StatusListener listener)
	{
		listenerList.add(StatusListener.class,listener);
	} 

	
	
	public final void removeStatusListener(StatusListener listener)
	{
		listenerList.remove(StatusListener.class,listener);
	} 

	
	
	public void propertiesChanged()
	{
		if(buffer == null)
			return;

		int _tabSize = buffer.getTabSize();
		char[] foo = new char[_tabSize];
		for(int i = 0; i < foo.length; i++)
			foo[i] = ' ';

		tabSize = painter.getStringWidth(new String(foo));

		charWidth = (int)Math.round(
			painter.getFont().getStringBounds(foo,0,1,
			painter.getFontRenderContext()).getWidth());

		String oldWrap = wrap;
		wrap = buffer.getStringProperty("wrap");
		hardWrap = wrap.equals("hard");
		softWrap = wrap.equals("soft");
		boolean oldWrapToWidth = wrapToWidth;
		int oldWrapMargin = wrapMargin;
		setMaxLineLength(buffer.getIntegerProperty("maxLineLen",0));

		boolean wrapSettingsChanged = !(wrap.equals(oldWrap)
			&& oldWrapToWidth == wrapToWidth
			&& oldWrapMargin == wrapMargin);

		if(displayManager != null && !bufferChanging
			&& !buffer.isLoading() && wrapSettingsChanged)
		{
			displayManager.invalidateScreenLineCounts();
			displayManager.notifyScreenLineChanges();
		}

		repaintMgr.setFastScroll(false);
		chunkCache.invalidateAll();
		gutter.repaint();
		painter.repaint();
	} 

	

	
	
	public final int getSelectionStart()
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getStart();
	} 

	
	
	public int getSelectionStart(int line)
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getStart(buffer,line);
	} 

	
	
	public final int getSelectionStartLine()
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getStartLine();
	} 

	
	
	public final void setSelectionStart(int selectionStart)
	{
		int selectionEnd = getSelectionCount() == 1 ? getSelection(0).getEnd() : caret;
		select(selectionStart,selectionEnd,true);
	} 

	
	
	public final int getSelectionEnd()
	{
		return getSelectionCount() == 1 ? getSelection(0).getEnd() : caret;

	} 

	
	
	public int getSelectionEnd(int line)
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getEnd(buffer,line);
	} 

	
	
	public final int getSelectionEndLine()
	{
		if(getSelectionCount() != 1)
			return caret;

		return getSelection(0).getEndLine();
	} 

	
	
	public final void setSelectionEnd(int selectionEnd)
	{
		select(getSelectionStart(),selectionEnd,true);
	} 

	
	
	public final int getMarkPosition()
	{
		Selection s = getSelectionAtOffset(caret);
		if(s == null)
			return caret;

		if(s.start == caret)
			return s.end;
		else if(s.end == caret)
			return s.start;
		else
			return caret;
	} 

	
	
	public final int getMarkLine()
	{
		if(getSelectionCount() != 1)
			return caretLine;

		Selection s = getSelection(0);
		if(s.start == caret)
			return s.endLine;
		else if(s.end == caret)
			return s.startLine;
		else
			return caretLine;
	} 

	
	
	public void select(int start, int end)
	{
		select(start,end,true);
	} 

	
	
	public void select(int start, int end, boolean doElectricScroll)
	{
		selectNone();

		int newStart, newEnd;
		if(start < end)
		{
			newStart = start;
			newEnd = end;
		}
		else
		{
			newStart = end;
			newEnd = start;
		}

		setSelection(new Selection.Range(newStart,newEnd));
		moveCaretPosition(end,doElectricScroll);
	} 

	
	
	public boolean isSelectionRectangular()
	{
		Selection s = getSelectionAtOffset(caret);
		return s != null && s instanceof Selection.Rect;
	} 

	

	

	static TextArea focusedComponent;

	
	final Segment lineSegment = new Segment();
	MouseInputAdapter mouseHandler;
	final ChunkCache chunkCache;
	final FastRepaintManager repaintMgr;
	DisplayManager displayManager;
	final SelectionManager selectionManager;
	boolean bufferChanging;

	int maxHorizontalScrollWidth;

	String wrap;
	boolean hardWrap;
	boolean softWrap;
	boolean wrapToWidth;
	int maxLineLen;
	int wrapMargin;
	float tabSize;
	int charWidth;

	boolean scrollBarsInitialized;

	
	final Point offsetXY;

	boolean lastLinePartial;

	boolean blink;
	

	
	
	final boolean isCaretVisible()
	{
		return blink && hasFocus();
	} 

	
	
	final boolean isStructureHighlightVisible()
	{
		return match != null
			&& hasFocus()
			&& displayManager.isLineVisible(match.startLine)
			&& displayManager.isLineVisible(match.endLine);
	} 

	
	void updateMaxHorizontalScrollWidth()
	{
		int max = chunkCache.getMaxHorizontalScrollWidth();

		if(max != maxHorizontalScrollWidth)
		{
			maxHorizontalScrollWidth = max;
			horizontal.setValues(Math.max(0,
				Math.min(maxHorizontalScrollWidth + charWidth
				- painter.getWidth(),
				-horizontalOffset)),
				painter.getWidth(),
				0,maxHorizontalScrollWidth
				+ charWidth);
			horizontal.setUnitIncrement(10);
			horizontal.setBlockIncrement(painter.getWidth());
		}
		else if (horizontal.getValue() != -horizontalOffset)
		{
			horizontal.setValue(-horizontalOffset);
		}
	} 

	
	void recalculateVisibleLines()
	{
		if(painter == null)
			return;
		int height = painter.getHeight();
		int lineHeight = painter.getFontMetrics().getHeight();
		if(lineHeight == 0)
			visibleLines = 0;
		else if(height <= 0)
		{
			visibleLines = 0;
			lastLinePartial = false;
		}
		else
		{
			visibleLines = height / lineHeight;
			lastLinePartial = (height % lineHeight != 0);
			if(lastLinePartial)
				visibleLines++;
		}

		chunkCache.recalculateVisibleLines();

		
		if(displayManager != null && buffer != null && !buffer.isLoading())
			setFirstLine(getFirstLine());

		updateScrollBar();
	} 

	
	void foldStructureChanged()
	{
		repaintMgr.setFastScroll(false);
		chunkCache.invalidateAll();
		recalculateLastPhysicalLine();
		repaint();
	} 

	
	
	void updateScrollBar()
	{
		if(buffer == null)
			return;

		if(Debug.SCROLL_DEBUG)
			Log.log(Log.DEBUG,this,"updateScrollBar(), slc="
				+ displayManager.getScrollLineCount());

		if(vertical != null && visibleLines != 0)
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"Vertical ok");
			int lineCount = displayManager.getScrollLineCount();
			int firstLine = getFirstLine();
			int visible = visibleLines - (lastLinePartial ? 1 : 0);

			vertical.setValues(firstLine,visible,0,lineCount);
			vertical.setUnitIncrement(2);
			vertical.setBlockIncrement(visible);
		}
	} 

	
	
	void _finishCaretUpdate()
	{
		if(!queuedCaretUpdate)
			return;

		try
		{
			if(match != null)
			{
				if(oldCaretLine < match.startLine)
					invalidateLineRange(oldCaretLine,match.endLine);
				else
					invalidateLineRange(match.startLine,oldCaretLine);
				match = null;
			}

			int newCaretScreenLine = chunkCache.getScreenLineOfOffset(caretLine,
				caret - buffer.getLineStartOffset(caretLine));
			if(caretScreenLine == -1)
				invalidateScreenLineRange(newCaretScreenLine,newCaretScreenLine);
			else
				invalidateScreenLineRange(caretScreenLine,newCaretScreenLine);
			caretScreenLine = newCaretScreenLine;

			invalidateSelectedLines();

			
			
			blink = true;
			caretTimer.restart();

			if(!displayManager.isLineVisible(caretLine))
			{
				if(caretLine < displayManager.getFirstVisibleLine()
					|| caretLine > displayManager.getLastVisibleLine())
				{
					int collapseFolds = buffer.getIntegerProperty(
						"collapseFolds",0);
					if(collapseFolds != 0)
					{
						displayManager.expandFolds(collapseFolds);
						displayManager.expandFold(caretLine,false);
					}
					else
						displayManager.expandAllFolds();
				}
				else
					displayManager.expandFold(caretLine,false);
			}

			if(queuedScrollMode == ELECTRIC_SCROLL)
				scrollToCaret(true);
			else if(queuedScrollMode == NORMAL_SCROLL)
				scrollToCaret(false);

			updateBracketHighlightWithDelay();
			if(queuedFireCaretEvent)
				fireCaretEvent();
		}
		
		
		finally
		{
			queuedCaretUpdate = queuedFireCaretEvent = false;
			queuedScrollMode = NO_SCROLL;
		}
	} 

	
	void invalidateStructureMatch()
	{
		if(match != null)
			invalidateLineRange(match.startLine,match.endLine);
	} 

	
	void startDragAndDrop(InputEvent evt, boolean copy)
	{
		TransferHandler transferHandler = getTransferHandler();
		if (transferHandler != null)
		{
			Log.log(Log.DEBUG,this,"Drag and drop callback");
			transferHandler.exportAsDrag(this,evt,
				copy ? TransferHandler.COPY
				: TransferHandler.MOVE);
		}
	} 

	
	void fireNarrowActive()
	{
		Object[] listeners = listenerList.getListenerList();
		for(int i = listeners.length - 2; i >= 0; i--)
		{
			if(listeners[i] == StatusListener.class)
			{
				try
				{
					((StatusListener)listeners[i+1])
						.narrowActive(this);
				}
				catch(Throwable t)
				{
					Log.log(Log.ERROR,this,t);
				}
			}
		}
	} 

	

	

	
	private static final Timer caretTimer;
	private static final Timer structureTimer;
	

	
	protected Cursor hiddenCursor;

	private final Gutter gutter;
	protected final TextAreaPainter painter;

	private final EventListenerList listenerList;
	private final MutableCaretEvent caretEvent;

	private boolean caretBlinks;
	private InputHandlerProvider inputHandlerProvider;

	private int physLastLine;
	private int screenLastLine;

	private int visibleLines;
	private int electricScroll;

	private int horizontalOffset;

	private boolean quickCopy;

	
	private final Box verticalBox;
	private final JScrollBar vertical;
	private final JScrollBar horizontal;

	protected JEditBuffer buffer;

	protected int caret;
	protected int caretLine;
	private int caretScreenLine;

	private final java.util.List<StructureMatcher> structureMatchers;
	private StructureMatcher.Match match;

	private int magicCaret;
	
	protected boolean multi;
	private boolean overwrite;
	private boolean rectangularSelectionMode;

	private boolean dndEnabled;
	private boolean dndInProgress;

	
	private boolean queuedCaretUpdate;
	private int queuedScrollMode;
	private boolean queuedFireCaretEvent;
	private int oldCaretLine;

	private boolean joinNonWordChars;
	

	
	
	private void invalidateSelectedLines()
	{
		
		invalidateLine(caretLine);

		for (Selection s : selectionManager.selection)
			invalidateLineRange(s.startLine,s.endLine);
	} 

	
	
	private void finishCaretUpdate(int oldCaretLine,
		int scrollMode, boolean fireCaretEvent)
	{
		queuedFireCaretEvent |= fireCaretEvent;
		queuedScrollMode = Math.max(scrollMode,queuedScrollMode);

		if(queuedCaretUpdate)
			return;

		this.oldCaretLine = oldCaretLine;
		queuedCaretUpdate = true;

		if(!buffer.isTransactionInProgress())
			_finishCaretUpdate();
		

		repaintMgr.setFastScroll(false);
	} 

	
	private void fireCaretEvent()
	{
		Object[] listeners = listenerList.getListenerList();
		for(int i = listeners.length - 2; i >= 0; i--)
		{
			if(listeners[i] == CaretListener.class)
			{
				try
				{
					((CaretListener)listeners[i+1]).caretUpdate(caretEvent);
				}
				catch(Throwable t)
				{
					Log.log(Log.ERROR,this,t);
				}
			}
		}
	} 

	
	private void fireScrollEvent(boolean vertical)
	{
		Object[] listeners = listenerList.getListenerList();
		for(int i = listeners.length - 2; i >= 0; i--)
		{
			if(listeners[i] == ScrollListener.class)
			{
				try
				{
					if(vertical)
						((ScrollListener)listeners[i+1]).scrolledVertically(this);
					else
						((ScrollListener)listeners[i+1]).scrolledHorizontally(this);
				}
				catch(Throwable t)
				{
					Log.log(Log.ERROR,this,t);
				}
			}
		}
	} 

	
	private void fireStatusChanged(int flag, boolean value)
	{
		Object[] listeners = listenerList.getListenerList();
		for(int i = listeners.length - 2; i >= 0; i--)
		{
			if(listeners[i] == StatusListener.class)
			{
				try
				{
					((StatusListener)listeners[i+1])
						.statusChanged(this,flag,value);
				}
				catch(Throwable t)
				{
					Log.log(Log.ERROR,this,t);
				}
			}
		}
	} 

	
	private void fireBracketSelected(int line, String text)
	{
		Object[] listeners = listenerList.getListenerList();
		for(int i = listeners.length - 2; i >= 0; i--)
		{
			if(listeners[i] == StatusListener.class)
			{
				try
				{
					((StatusListener)listeners[i+1])
						.bracketSelected(this,line,text);
				}
				catch(Throwable t)
				{
					Log.log(Log.ERROR,this,t);
				}
			}
		}
	} 

	
	private void _changeLine(boolean select, int newCaret)
	{
		if(select)
		{
			RectParams params = getRectParams(caret,newCaret);
			int extraStartVirt;
			int extraEndVirt;
			if(params == null)
			{
				extraStartVirt = 0;
				extraEndVirt = 0;
			}
			else
			{
				extraStartVirt = params.extraStartVirt;
				extraEndVirt = params.extraEndVirt;
				newCaret = params.newCaret;
			}
			extendSelection(caret,newCaret,extraStartVirt,extraEndVirt);
		}
		else if(!multi)
			selectNone();

		moveCaretPosition(newCaret);
	}

	
	
	private boolean lineContainsSpaceAndTabs(int lineIndex)
	{
		getLineText(lineIndex,lineSegment);

		for(int j = 0; j < lineSegment.count; j++)
		{
			switch(lineSegment.array[lineSegment.offset + j])
			{
			case ' ':
			case '\t':
				break;
			default:
				return false;
			}
		}
		return true;
	}

	
	protected void insert(String str, boolean indent)
	{
		try
		{
			
			
			if(overwrite || indent)
				buffer.beginCompoundEdit();

			if(overwrite)
			{
				int caretLineEnd = getLineEndOffset(caretLine);
				if(caretLineEnd - caret > 1)
					buffer.remove(caret,1);
			}

			buffer.insert(caret,str);

			if(indent)
				buffer.indentLine(caretLine,true);
		}
		finally
		{
			if(overwrite || indent)
				buffer.endCompoundEdit();
		}
	} 

	
	private void insertTab()
	{
		int tabSize = buffer.getTabSize();
		if(buffer.getBooleanProperty("noTabs"))
		{
			int lineStart = getLineStartOffset(caretLine);

			String line = getText(lineStart,caret - lineStart);

			int pos = 0;

			for(int i = 0; i < line.length(); i++)
			{
				switch(line.charAt(pos))
				{
				case '\t':
					pos = 0;
					break;
				default:
					if(++pos >= tabSize)
						pos = 0;
					break;
				}
			}

			replaceSelection(StandardUtilities.createWhiteSpace(
				tabSize - pos,0));
		}
		else
			replaceSelection("\t");
	} 

	
	protected void userInputTab()
	{
		if(getSelectionCount() == 1)
		{
			Selection sel = getSelection(0);
			if(sel instanceof Selection.Rect ||
				(sel.startLine == sel.endLine
				&& (sel.start != buffer.getLineStartOffset(sel.startLine)
				|| sel.end != buffer.getLineEndOffset(sel.startLine) - 1)))
			{
				insertTab();
			}
			else
				shiftIndentRight();
		}
		else if(getSelectionCount() != 0)
			shiftIndentRight();
		else
			insertTab();
	} 

	
	
	protected boolean doWordWrap(boolean spaceInserted)
	{
		if(!hardWrap || maxLineLen <= 0)
			return false;

		buffer.getLineText(caretLine,lineSegment);

		int start = getLineStartOffset(caretLine);
		int end = getLineEndOffset(caretLine);
		int len = end - start - 1;

		int caretPos = caret - start;

		
		
		for(int i = caretPos; i < len; i++)
		{
			char ch = lineSegment.array[lineSegment.offset + i];
			if(ch != ' ' && ch != '\t')
				return false;
		}

		int tabSize = buffer.getTabSize();

		String wordBreakChars = buffer.getStringProperty("wordBreakChars");

		int lastInLine = 0; 
		int logicalLength = 0; 
		int lastWordOffset = -1;
		boolean lastWasSpace = true;
		for(int i = 0; i < caretPos; i++)
		{
			char ch = lineSegment.array[lineSegment.offset + i];
			if(ch == '\t')
			{
				logicalLength += tabSize - (logicalLength % tabSize);
				if(!lastWasSpace && logicalLength <= maxLineLen)
				{
					lastInLine = i;
					lastWordOffset = i;
					lastWasSpace = true;
				}
			}
			else if(ch == ' ')
			{
				logicalLength++;
				if(!lastWasSpace &&
					logicalLength <= maxLineLen + 1)
				{
					lastInLine = i;
					lastWordOffset = i;
					lastWasSpace = true;
				}
			}
			else if(wordBreakChars != null && wordBreakChars.indexOf(ch) != -1)
			{
				logicalLength++;
				if(!lastWasSpace && logicalLength <= maxLineLen)
				{
					lastInLine = i;
					lastWordOffset = i;
					lastWasSpace = true;
				}
			}
			else
			{
				lastInLine = i;
				logicalLength++;
				lastWasSpace = false;
			}
		}

		boolean returnValue;

		int insertNewLineAt;
		if(spaceInserted && logicalLength == maxLineLen
			&& lastInLine == caretPos - 1)
		{
			insertNewLineAt = caretPos;
			returnValue = true;
		}
		else if(logicalLength >= maxLineLen && lastWordOffset != -1)
		{
			insertNewLineAt = lastWordOffset;
			returnValue = false;
		}
		else
			return false;

		try
		{
			buffer.beginCompoundEdit();
			buffer.insert(start + insertNewLineAt,"\n");
			
			
			buffer.indentLine(caretLine,true);
		}
		finally
		{
			buffer.endCompoundEdit();
		}

		
		return returnValue;
	} 

	
	private static void updateBracketHighlightWithDelay()
	{
		structureTimer.stop();
		structureTimer.start();
	} 

	
	private void updateStructureHighlight()
	{
		if(!painter.isStructureHighlightEnabled()
			&& !gutter.isStructureHighlightEnabled())
			return;

		for (StructureMatcher matcher : structureMatchers)
		{
			match = matcher.getMatch(this);
			if(match != null)
				break;
		}

		if(match != null)
		{
			if(caretLine < match.startLine)
				invalidateLineRange(caretLine,match.endLine);
			else
				invalidateLineRange(match.startLine,caretLine);

			if(!displayManager.isLineVisible(match.startLine)
				|| chunkCache.getScreenLineOfOffset(
				match.startLine,match.start - getLineStartOffset(match.startLine))
				== -1)
			{
				showStructureStatusMessage(match.startLine < caretLine);
			}
		}
	} 

	
	private void showStructureStatusMessage(boolean backward)
	{
		String text = buffer.getLineText(match.startLine).trim();
		if(backward && match.startLine != 0 && text.length() == 1)
		{
			switch(text.charAt(0))
			{
			case '{': case '}':
			case '[': case ']':
			case '(': case ')':
				text = buffer.getLineText(match.startLine - 1)
					.trim() + ' ' + text;
				break;
			}
		}

		
		fireBracketSelected(match.startLine + 1,text.replace('\t',' '));
	} 

	
	void recalculateLastPhysicalLine()
	{
		int oldScreenLastLine = screenLastLine;
		for(int i = visibleLines - 1; i >= 0; i--)
		{
			ChunkCache.LineInfo info = chunkCache.getLineInfo(i);
			if(info.physicalLine != -1)
			{
				physLastLine = info.physicalLine;
				screenLastLine = i;
				break;
			}
		}
		invalidateScreenLineRange(oldScreenLastLine,screenLastLine);
	} 

	
	static class RectParams
	{
		final int extraStartVirt;
		final int extraEndVirt;
		final int newCaret;

		RectParams(int extraStartVirt, int extraEndVirt, int newCaret)
		{
			this.extraStartVirt = extraStartVirt;
			this.extraEndVirt = extraEndVirt;
			this.newCaret = newCaret;
		}
	}

	
	private RectParams getRectParams(int caret, int newCaret)
	{
		Selection s = getSelectionAtOffset(caret);
		int virtualWidth;
		if(s instanceof Selection.Rect)
		{
			if(caret == s.end)
			{
				virtualWidth = buffer.getVirtualWidth(
					s.endLine,s.end - getLineStartOffset(
					s.endLine)) + ((Selection.Rect)s).extraEndVirt;
			}
			else
			{
				virtualWidth = buffer.getVirtualWidth(
					s.startLine,s.start - getLineStartOffset(
					s.startLine)) + ((Selection.Rect)s).extraStartVirt;
			}
		}
		else if(rectangularSelectionMode)
		{
			virtualWidth = buffer.getVirtualWidth(
				caretLine,caret - buffer.getLineStartOffset(caretLine));
		}
		else
			return null;

		int newLine = getLineOfOffset(newCaret);
		int[] totalVirtualWidth = new int[1];
		int newOffset = buffer.getOffsetOfVirtualColumn(newLine,
			virtualWidth,totalVirtualWidth);
		if(newOffset == -1)
		{
			int extraVirt = virtualWidth - totalVirtualWidth[0];
			newCaret = getLineEndOffset(newLine) - 1;

			boolean bias;
			if(s == null)
				bias = newCaret < caret;
			else if(s.start == caret)
				bias = newCaret <= s.end;
			else if(s.end == caret)
				bias = newCaret <= s.start;
			else
				bias = false;

			RectParams returnValue;
			if(bias)
				returnValue = new RectParams(extraVirt,0,newCaret);
			else
				returnValue = new RectParams(0,extraVirt,newCaret);
			return returnValue;
		}
		else
		{
			return new RectParams(0,0,getLineStartOffset(newLine)
				+ newOffset);
		}
	} 

	
	private void delete(boolean forward)
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(getSelectionCount() != 0)
		{
			Selection[] selections = getSelection();
			for(int i = 0; i < selections.length; i++)
			{
				Selection s = selections[i];
				if(s instanceof Selection.Rect)
				{
					Selection.Rect r = (Selection.Rect)s;
					int startColumn = r.getStartColumn(buffer);
					if(startColumn == r.getEndColumn(buffer))
					{
						if(!forward && startColumn == 0)
							getToolkit().beep();
						else
							tallCaretDelete(r,forward);
					}
					else
						setSelectedText(s,null);
				}
				else
					setSelectedText(s,null);
			}
		}
		else if(forward)
		{
			if(caret == buffer.getLength())
			{
				getToolkit().beep();
				return;
			}

			buffer.remove(caret,1);
		}
		else
		{
			if(caret == 0)
			{
				getToolkit().beep();
				return;
			}

			buffer.remove(caret - 1,1);
		}
	} 

	
	private void tallCaretDelete(Selection.Rect s, boolean forward)
	{
		try
		{
			buffer.beginCompoundEdit();

			int[] width = new int[1];

			int startCol = s.getStartColumn(buffer);
			int startLine = s.startLine;
			int endLine = s.endLine;
			for(int i = startLine; i <= endLine; i++)
			{
				int offset = buffer.getOffsetOfVirtualColumn(
					i,startCol,width);
				if(offset == -1)
				{
					if(width[0] == startCol)
						offset = getLineLength(i);
					else
					{
						if(i == startLine && !forward)
							shiftTallCaretLeft(s);
						continue;
					}
				}
				offset += buffer.getLineStartOffset(i);
				if(forward)
				{
					if(offset != buffer.getLineEndOffset(i) - 1)
						buffer.remove(offset,1);
				}
				else
					buffer.remove(offset-1,1);
			}
		}
		finally
		{
			buffer.endCompoundEdit();
		}
	} 

	
	private void shiftTallCaretLeft(Selection.Rect s)
	{
		removeFromSelection(s);
		addToSelection(new Selection.Rect(
			buffer,
			s.getStartLine(),s.getStartColumn(buffer) - 1,
			s.getEndLine(),s.getEndColumn(buffer) - 1));
	} 

	
	private void setMaxLineLength(int maxLineLen)
	{
		this.maxLineLen = maxLineLen;

		if(maxLineLen <= 0)
		{
			if(softWrap)
			{
				wrapToWidth = true;
				wrapMargin = painter.getWidth() - charWidth * 3;
			}
			else
			{
				wrapToWidth = false;
				wrapMargin = 0;
			}
		}
		else
		{
			
			char[] foo = new char[maxLineLen];
			for(int i = 0; i < foo.length; i++)
			{
				foo[i] = ' ';
			}
			wrapToWidth = false;
			wrapMargin = (int)painter.getFont().getStringBounds(
				foo,0,foo.length,
				painter.getFontRenderContext())
				.getWidth();
		}
	} 

	
	
	protected int addExplicitFold(int caretStart, int caretEnd, int lineStart, int lineEnd)
	{
		
		
		
		
		
		int startCaret = caretStart < buffer.getLength() ? caretStart + 1 : caretStart;
		int endCaret = caretEnd > 0 ? caretEnd - 1 : caretEnd;

		String startLineComment = buffer.getContextSensitiveProperty(startCaret,"lineComment");
		String startCommentStart = buffer.getContextSensitiveProperty(startCaret,"commentStart");
		String startCommentEnd = buffer.getContextSensitiveProperty(startCaret,"commentEnd");
		String endLineComment = buffer.getContextSensitiveProperty(endCaret,"lineComment");
		String endCommentStart = buffer.getContextSensitiveProperty(endCaret,"commentStart");
		String endCommentEnd = buffer.getContextSensitiveProperty(endCaret,"commentEnd");

		String start;
		int caretBack = 1;
		if(startLineComment != null)
			start = startLineComment + "{{{ ";
		else if(startCommentStart != null && startCommentEnd != null)
		{
			start = startCommentStart + "{{{  " + startCommentEnd;
			caretBack = 1 + startCommentStart.length();
		}
		else
			start = "{{{ ";

		if (startLineComment != null) {
			
			
			int nextLineOffset = buffer.getLineStartOffset(lineStart+1);
			if (nextLineOffset - caretStart != 1)
				start += "\n";
		}
		else
		{
			
			start += "\n";
		}

		String end;
		if(endLineComment != null)
			end = endLineComment + "}}}";
		else if(endCommentStart != null && endCommentEnd != null)
			end = endCommentStart + "}}}" + endCommentEnd;
		else
			end = "}}}";

		String line = buffer.getLineText(lineStart);
		String whitespace = line.substring(0,
			StandardUtilities.getLeadingWhiteSpace(line));

		if (endLineComment != null)
		{
			
			
			
			int nextLineOffset = buffer.getLineStartOffset(lineEnd+1);
			if (nextLineOffset - caretEnd != 1)
				end += "\n";
		}
		else
		{
			
			end += "\n";
		}

		if(caretEnd == buffer.getLineStartOffset(lineEnd))
			buffer.insert(caretEnd,end);
		else
		{
			String lineText = buffer.getText(caretEnd - 1, 1);
			if (Character.isWhitespace(lineText.charAt(0)))
				buffer.insert(caretEnd, end);
			else
				buffer.insert(caretEnd,' ' + end);
		}

		buffer.insert(caretStart,start + whitespace);

		return caretBack;
	} 
	

	

	
	static class CaretBlinker implements ActionListener
	{
		
		public void actionPerformed(ActionEvent evt)
		{
			if(focusedComponent != null && focusedComponent.hasFocus())
				focusedComponent.blinkCaret();
		} 
	} 

	
	class MutableCaretEvent extends CaretEvent
	{
		
		MutableCaretEvent()
		{
			super(TextArea.this);
		} 

		
		public int getDot()
		{
			return getCaretPosition();
		} 

		
		public int getMark()
		{
			return getMarkPosition();
		} 
	} 

	
	class AdjustHandler implements AdjustmentListener
	{
		
		public void adjustmentValueChanged(AdjustmentEvent evt)
		{
			if(!scrollBarsInitialized)
				return;

			if(evt.getAdjustable() == vertical)
				setFirstLine(vertical.getValue());
			else
				setHorizontalOffset(-horizontal.getValue());
		} 
	} 

	
	class FocusHandler implements FocusListener
	{
		
		public void focusGained(FocusEvent evt)
		{
			if(bufferChanging)
				return;

			if(match != null)
			{
				if(caretLine < match.startLine)
					invalidateLineRange(caretLine,match.endLine);
				else
					invalidateLineRange(match.startLine,caretLine);
			}
			else
				invalidateLine(caretLine);

			focusedComponent = TextArea.this;
		} 

		
		public void focusLost(FocusEvent evt)
		{
			if(!isShowing())
				return;

			if(match != null)
			{
				if(caretLine < match.startLine)
					invalidateLineRange(caretLine,match.endLine);
				else
					invalidateLineRange(match.startLine,caretLine);
			}
			else
				invalidateLine(caretLine);
		} 
	} 

	
	class MouseWheelHandler implements MouseWheelListener
	{
		public void mouseWheelMoved(MouseWheelEvent e)
		{
			
			if(e.isAltDown())
			{
				boolean select = e.isShiftDown()
					|| e.isControlDown();
				if(e.getWheelRotation() < 0)
					goToPrevLine(select);
				else
					goToNextLine(select);
			}
			else if(e.isShiftDown())
			{
				if(e.getWheelRotation() > 0)
					scrollDownPage();
				else
					scrollUpPage();
			}
			else if(e.isControlDown())
			{
				setFirstLine(getFirstLine()
					+ e.getWheelRotation());
			}
			else if(e.getScrollType()
				== MouseWheelEvent.WHEEL_UNIT_SCROLL)
			{
				setFirstLine(getFirstLine()
					+ e.getUnitsToScroll());
			}
			else
			{
				setFirstLine(getFirstLine()
					+ 3 * e.getWheelRotation());
			}
		}
	} 

	

	
	static
	{
		caretTimer = new Timer(500,new CaretBlinker());
		caretTimer.setInitialDelay(500);
		caretTimer.start();

		structureTimer = new Timer(100,new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				if(focusedComponent != null)
					focusedComponent.updateStructureHighlight();
			}
		});
		structureTimer.setInitialDelay(100);
		structureTimer.setRepeats(false);
	} 

	
	public static void main(String[] args)
	{
		JFrame frame = new JFrame();
		TextArea text = new TextArea();
		frame.getContentPane().add(text);
		frame.pack();
		frame.setVisible(true);
	} 
}
