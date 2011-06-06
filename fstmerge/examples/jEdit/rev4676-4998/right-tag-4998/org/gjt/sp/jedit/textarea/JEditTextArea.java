

package org.gjt.sp.jedit.textarea;


import java.awt.*;
import java.awt.event.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Vector;
import javax.swing.*;
import javax.swing.border.*;
import javax.swing.event.*;
import javax.swing.plaf.metal.MetalLookAndFeel;
import javax.swing.text.Position;
import javax.swing.text.Segment;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.gui.*;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.util.Log;



public class JEditTextArea extends JComponent
{
	
	
	public JEditTextArea(View view)
	{
		enableEvents(AWTEvent.FOCUS_EVENT_MASK | AWTEvent.KEY_EVENT_MASK);

		this.view = view;

		
		selection = new Vector();
		chunkCache = new ChunkCache(this);
		painter = new TextAreaPainter(this);
		gutter = new Gutter(view,this);
		bufferHandler = new BufferChangeHandler();
		listenerList = new EventListenerList();
		caretEvent = new MutableCaretEvent();
		bracketLine = bracketPosition = -1;
		blink = true;
		lineSegment = new Segment();
		returnValue = new Point();
		runnables = new ArrayList();
		

		
		setLayout(new ScrollLayout());
		add(LEFT,gutter);
		add(CENTER,painter);

		
		verticalBox = new Box(BoxLayout.X_AXIS);
		verticalBox.add(vertical = new JScrollBar(JScrollBar.VERTICAL));
		add(RIGHT,verticalBox);
		add(BOTTOM,horizontal = new JScrollBar(JScrollBar.HORIZONTAL));

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

		mouseHandler = new MouseHandler();
		painter.addMouseListener(mouseHandler);
		painter.addMouseMotionListener(mouseHandler);

		addFocusListener(new FocusHandler());
		

		
		
		
		focusedComponent = this;
	} 

	
	
	public void dispose()
	{
		displayManager.dispose();
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

	
	
	public final Buffer getBuffer()
	{
		return buffer;
	} 

	
	
	public void setBuffer(Buffer buffer)
	{
		if(this.buffer == buffer)
			return;

		try
		{
			bufferChanging = true;

			if(this.buffer != null)
			{
				
				

				selectNone();
				caretLine = caret = caretScreenLine = 0;
				bracketLine = bracketPosition = -1;

				displayManager.dispose();
				this.buffer.removeBufferChangeListener(bufferHandler);
			}
			this.buffer = buffer;

			buffer.addBufferChangeListener(bufferHandler);
			bufferHandlerInstalled = true;

			chunkCache.setBuffer(buffer);
			propertiesChanged();
			displayManager = new DisplayManager(buffer,this);
			
			
			
			
			
			if(buffer.isLoaded())
			{
				displayManager.firstLine.reset();
				displayManager.scrollLineCount.reset();
			}

			if(buffer.isLoaded())
				recalculateLastPhysicalLine();

			maxHorizontalScrollWidth = 0;

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

	
	
	public final JPopupMenu getRightClickPopup()
	{
		return popup;
	} 

	
	
	public final void setRightClickPopup(JPopupMenu popup)
	{
		this.popup = popup;
	} 

	

	

	public void scrollTest(boolean paint)
	{
		Image im = painter.createImage(painter.getWidth(),painter.getHeight());
		Graphics gfx = im.getGraphics();
		gfx.setFont(painter.getFont());
		gfx.setColor(painter.getForeground());
		gfx.clipRect(0,0,painter.getWidth(),painter.getHeight());
		long start = System.currentTimeMillis();
		for(int i = 0; i < displayManager.getScrollLineCount(); i++)
		{
			setFirstLine(i);
			if(!paint)
				chunkCache.getLineInfo(visibleLines - 1);
			else
				painter.paintComponent(gfx);
		}
		System.err.println(System.currentTimeMillis() - start);
	}

	
	
	public final int getFirstLine()
	{
		return displayManager.firstLine.scrollLine
			+ displayManager.firstLine.skew;
	} 

	
	public Exception trace;
	
	public void setFirstLine(int firstLine)
	{
		if(Debug.SCROLL_DEBUG)
		{
			Log.log(Log.DEBUG,this,"setFirstLine() from "
				+ getFirstLine() + " to " + firstLine);
		}

		
		int max = displayManager.getScrollLineCount() - visibleLines
			+ (lastLinePartial ? 1 : 0);
		if(firstLine > max)
			firstLine = max;
		if(firstLine < 0)
			firstLine = 0;
		

		int oldFirstLine = getFirstLine();
		if(firstLine == oldFirstLine)
			return;

		trace = new Exception();

		if(firstLine >= oldFirstLine + visibleLines)
		{
			displayManager.firstLine.scrollDown(firstLine - oldFirstLine);
			chunkCache.invalidateAll();
		}
		else if(firstLine <= oldFirstLine - visibleLines)
		{
			displayManager.firstLine.scrollUp(oldFirstLine - firstLine);
			chunkCache.invalidateAll();
		}
		else if(firstLine > oldFirstLine)
		{
			displayManager.firstLine.scrollDown(firstLine - oldFirstLine);
			chunkCache.scrollDown(firstLine - oldFirstLine);
		}
		else if(firstLine < oldFirstLine)
		{
			displayManager.firstLine.scrollUp(oldFirstLine - firstLine);
			chunkCache.scrollUp(oldFirstLine - firstLine);
		}

		displayManager.firstLine.changed();

		maxHorizontalScrollWidth = 0;

		
		

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
			Log.log(Log.DEBUG,this,"setFirstPhysicalLine()");

		
		
		

		int amount = (physFirstLine - displayManager.firstLine.physicalLine);

		if(amount == 0)
			return;

		int oldFirstLine = getFirstLine();

		if(amount > 0)
			displayManager.firstLine.physDown(amount,skew);
		else if(amount < 0)
			displayManager.firstLine.physUp(-amount,skew);

		int firstLine = getFirstLine();

		if(firstLine == oldFirstLine)
			;
		else if(firstLine >= oldFirstLine + visibleLines
			|| firstLine <= oldFirstLine - visibleLines)
		{
			chunkCache.invalidateAll();
		}
		else if(firstLine > oldFirstLine)
		{
			chunkCache.scrollDown(firstLine - oldFirstLine);
		}
		else if(firstLine < oldFirstLine)
		{
			chunkCache.scrollUp(oldFirstLine - firstLine);
		}

		displayManager.firstLine.changed();

		maxHorizontalScrollWidth = 0;

		
		

		repaint();

		fireScrollEvent(true);
	} 

	
	
	public final int getLastPhysicalLine()
	{
		return physLastLine;
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
		if(horizontalOffset != horizontal.getValue())
			updateScrollBars();
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

	
	
	public void scrollTo(int line, int offset, boolean doElectricScroll)
	{
		if(Debug.SCROLL_DEBUG)
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

		int _electricScroll = (doElectricScroll
			&& visibleLines - 1 > electricScroll * 2
			? electricScroll : 0); 

		if(visibleLines == 0)
		{
			setFirstPhysicalLine(line,_electricScroll);
			
			return;
		}

		
		int firstLine = getFirstLine();
		int screenLine = getScreenLineOfOffset(buffer.getLineStartOffset(line) + offset);
		int visibleLines = getVisibleLines();
		if(screenLine == -1)
		{
			int prevLine = displayManager.getPrevVisibleLine(getFirstPhysicalLine());
			int nextLine = displayManager.getNextVisibleLine(getLastPhysicalLine());
			if(line == prevLine)
			{
				setFirstLine(getFirstLine() - _electricScroll
					- displayManager.getScreenLineCount(
					prevLine));
			}
			else if(line == nextLine)
			{
				setFirstLine(getFirstLine() + _electricScroll
					+ displayManager.getScreenLineCount(
					nextLine));
			}
			else
			{
				setFirstPhysicalLine(line,-visibleLines / 2);
			}
		}
		else if(screenLine < _electricScroll)
		{
			setFirstLine(getFirstLine() - _electricScroll + screenLine);
		}
		else if(screenLine > visibleLines - _electricScroll
			- (lastLinePartial ? 2 : 1))
		{
			setFirstLine(getFirstLine() + _electricScroll - visibleLines + screenLine + (lastLinePartial ? 2 : 1));
		} 

		
		Point point = offsetToXY(line,offset,returnValue);
		if(point == null)
			Log.log(Log.ERROR,this,"BUG: screenLine=" + screenLine
				+ ",visibleLines=" + visibleLines
				+ ",firstPhysicalLine=" + getFirstPhysicalLine()
				+ ",lastPhysicalLine=" + getLastPhysicalLine());
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

	
	
	public Point offsetToXY(int line, int offset, Point retVal)
	{
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
		if(!buffer.isLoaded())
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
			|| !buffer.isLoaded()
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
		if(!isShowing() || !buffer.isLoaded())
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

	
	
	public void invalidateSelectedLines()
	{
		
		invalidateLine(caretLine);

		for(int i = 0; i < selection.size(); i++)
		{
			Selection s = (Selection)selection.elementAt(i);
			invalidateLineRange(s.startLine,s.endLine);
		}
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
		setSelection(new Selection.Range(0,buffer.getLength()));
		moveCaretPosition(buffer.getLength(),true);
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

	
	
	public Selection selectToMatchingBracket(int position, boolean quickCopy)
	{
		int positionLine = buffer.getLineOfOffset(position);
		int lineOffset = position - buffer.getLineStartOffset(positionLine);
		if(lineOffset == getLineLength(positionLine))
			position--;

		int bracket = TextUtilities.findMatchingBracket(buffer,positionLine,lineOffset);

		if(bracket != -1)
		{
			Selection s;

			if(bracket < position)
			{
				if(!quickCopy)
					moveCaretPosition(position,false);
				s = new Selection.Range(++bracket,position);
			}
			else
			{
				if(!quickCopy)
					moveCaretPosition(position + 1,false);
				s = new Selection.Range(position + 1,bracket);
			}

			if(!multi && !quickCopy)
				selectNone();

			addToSelection(s);
			return s;
		}

		return null;
	} 

	
	
	public void selectToMatchingBracket()
	{
		selectToMatchingBracket(caret,false);
	} 

	
	
	public void narrowToMatchingBracket()
	{
		narrowToMatchingBracket(caret);
	} 

	
	
	public void narrowToMatchingBracket(int position)
	{
		int line = buffer.getLineOfOffset(position);
		int offset = position - buffer.getLineStartOffset(position);

		int bracket = TextUtilities.findMatchingBracket(buffer,line,offset);

		if(bracket != -1)
		{
			int start = Math.min(bracketLine,line);
			int end = Math.max(bracketLine,line);

			displayManager.narrow(start,end);
		}
	} 

	
	
	public void selectBlock()
	{
		String openBrackets = "([{";
		String closeBrackets = ")]}";

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

		
		int count = 1;
		char openBracket = '\0';
		char closeBracket = '\0';

		
		if(start == 0)
		{
			getToolkit().beep();
			return;
		}

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
		else
		{
forward_scan:		do
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
		}

		s = new Selection.Range(start,end);
		if(multi)
			addToSelection(s);
		else
			setSelection(s);
		moveCaretPosition(end);
	} 

	
	
	public boolean lineInBracketScope(int line)
	{
		if(bracketPosition == -1)
			return false;

		if(bracketLine < caretLine)
			return (line >= bracketLine && line <= caretLine);
		else
			return (line <= bracketLine && line >= caretLine);
	} 

	
	
	public final void invertSelection()
	{
		Selection[] newSelection = new Selection[selection.size() + 1];
		int lastOffset = 0;
		for(int i = 0; i < selection.size(); i++)
		{
			Selection s = (Selection)selection.elementAt(i);
			newSelection[i] = new Selection.Range(lastOffset,
				s.getStart());
			lastOffset = s.getEnd();
		}
		newSelection[selection.size()] = new Selection.Range(
			lastOffset,buffer.getLength());
		setSelection(newSelection);
	} 

	
	
	public int getSelectionCount()
	{
		return selection.size();
	} 

	
	
	public Selection[] getSelection()
	{
		Selection[] sel = new Selection[selection.size()];
		selection.copyInto(sel);
		return sel;
	} 

	
	
	public void selectNone()
	{
		setSelection((Selection)null);
	} 

	
	
	public void setSelection(Selection[] selection)
	{
		
		invalidateSelectedLines();

		this.selection.removeAllElements();

		if(selection != null)
		{
			for(int i = 0; i < selection.length; i++)
				_addToSelection(selection[i]);
		}

		fireCaretEvent();
	} 

	
	
	public void setSelection(Selection selection)
	{
		invalidateSelectedLines();
		this.selection.removeAllElements();

		if(selection != null)
			_addToSelection(selection);

		fireCaretEvent();
	} 

	
	
	public void addToSelection(Selection[] selection)
	{
		if(selection != null)
		{
			for(int i = 0; i < selection.length; i++)
				_addToSelection(selection[i]);
		}

		
		invalidateLine(caretLine);

		fireCaretEvent();
	} 

	
	
	public void addToSelection(Selection selection)
	{
		_addToSelection(selection);

		
		invalidateLine(caretLine);

		fireCaretEvent();
	} 

	
	
	public Selection getSelectionAtOffset(int offset)
	{
		if(selection != null)
		{
			for(int i = 0; i < selection.size(); i++)
			{
				Selection s = (Selection)selection.elementAt(i);
				if(offset >= s.start && offset <= s.end)
					return s;
			}
		}

		return null;
	} 

	
	
	public void removeFromSelection(Selection sel)
	{
		selection.removeElement(sel);
		invalidateLineRange(sel.startLine,sel.endLine);

		
		invalidateLine(caretLine);

		fireCaretEvent();
	} 

	
	public void removeFromSelection(int offset)
	{
		Selection sel = getSelectionAtOffset(offset);
		if(sel == null)
			return;

		selection.removeElement(sel);
		invalidateLineRange(sel.startLine,sel.endLine);

		
		invalidateLine(caretLine);

		fireCaretEvent();
	} 

	
	
	public void resizeSelection(int offset, int end, int extraEndVirt,
		boolean rect)
	{
		Selection s = getSelectionAtOffset(offset);
		if(s != null)
		{
			invalidateLineRange(s.startLine,s.endLine);
			selection.removeElement(s);
		}

		boolean reversed = false;
		if(end < offset)
		{
			int tmp = offset;
			offset = end;
			end = tmp;
			reversed = true;
		}

		Selection newSel;
		if(rect)
		{
			Selection.Rect rectSel = new Selection.Rect(offset,end);
			if(reversed)
				rectSel.extraStartVirt = extraEndVirt;
			else
				rectSel.extraEndVirt = extraEndVirt;
			newSel = rectSel;
		}
		else
			newSel = new Selection.Range(offset,end);

		_addToSelection(newSel);
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
			selection.removeElement(s);

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

		_addToSelection(s);
		fireCaretEvent();

		if(rectangularSelectionMode && extraEndVirt != 0)
		{
			int line = getLineOfOffset(end);
			scrollTo(line,getLineLength(line) + extraEndVirt,false);
		}
	} 

	
	
	public String getSelectedText(Selection s)
	{
		StringBuffer buf = new StringBuffer();
		s.getText(buffer,buf);
		return buf.toString();
	} 

	
	
	public String getSelectedText(String separator)
	{
		if(selection.size() == 0)
			return null;

		StringBuffer buf = new StringBuffer();
		for(int i = 0; i < selection.size(); i++)
		{
			if(i != 0)
				buf.append(separator);

			((Selection)selection.elementAt(i)).getText(buffer,buf);
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
		if(!isEditable())
		{
			throw new InternalError("Text component"
				+ " read only");
		}

		Selection[] selection = getSelection();
		if(selection.length == 0)
		{
			
			buffer.insert(caret,selectedText);
		}
		else
		{
			try
			{
				int newCaret = -1;

				buffer.beginCompoundEdit();

				for(int i = 0; i < selection.length; i++)
				{
					newCaret = selection[i].setText(buffer,selectedText);
				}

				moveCaretPosition(newCaret);
			}
			finally
			{
				buffer.endCompoundEdit();
			}
		}

		selectNone();
	} 

	
	
	public int[] getSelectedLines()
	{
		if(selection.size() == 0)
			return new int[] { caretLine };

		Integer line;

		Hashtable hash = new Hashtable();
		for(int i = 0; i < selection.size(); i++)
		{
			Selection s = (Selection)selection.elementAt(i);
			int endLine = (s.end == getLineStartOffset(s.endLine)
				? s.endLine - 1
				: s.endLine);

			for(int j = s.startLine; j <= endLine; j++)
			{
				line = new Integer(j);
				hash.put(line,line);
			}
		}

		int[] returnValue = new int[hash.size()];
		int i = 0;

		Enumeration keys = hash.keys();
		while(keys.hasMoreElements())
		{
			line = (Integer)keys.nextElement();
			returnValue[i++] = line.intValue();
		}

		Arrays.sort(returnValue);

		return returnValue;
	} 

	
	
	public void showSelectLineRangeDialog()
	{
		new SelectLineRange(view);
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
		int offset = getScreenLineStartOffset(visibleLines / 2);
		if(offset == -1)
			getToolkit().beep();
		else
			setCaretPosition(offset);
	} 

	
	
	public void setCaretPosition(int newCaret)
	{
		invalidateSelectedLines();
		selection.removeAllElements();
		moveCaretPosition(newCaret,true);
	} 

	
	
	public void setCaretPosition(int newCaret, boolean doElectricScroll)
	{
		invalidateSelectedLines();
		selection.removeAllElements();
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

	
	public int NO_SCROLL = 0;
	public int NORMAL_SCROLL = 1;
	public int ELECTRIC_SCROLL = 2;
	
	public void moveCaretPosition(int newCaret, int scrollMode)
	{
		if(newCaret < 0 || newCaret > buffer.getLength())
		{
			throw new IllegalArgumentException("caret out of bounds: "
				+ newCaret);
		}

		if(caret == newCaret)
		{
			if(bracketLine != -1)
			{
				invalidateLineRange(bracketLine,caretLine);
				bracketLine = bracketPosition = -1;
			}

			if(scrollMode == NORMAL_SCROLL)
				finishCaretUpdate(false,false);
			else if(scrollMode == ELECTRIC_SCROLL)
				finishCaretUpdate(true,false);
		}
		else
		{
			int newCaretLine = getLineOfOffset(newCaret);

			magicCaret = -1;

			if(!displayManager.isLineVisible(newCaretLine))
			{
				if(newCaretLine < displayManager.getFirstVisibleLine()
					|| newCaretLine > displayManager.getLastVisibleLine())
				{
					int collapseFolds = buffer.getIntegerProperty(
						"collapseFolds",0);
					if(collapseFolds != 0)
					{
						displayManager.expandFolds(collapseFolds);
						displayManager.expandFold(newCaretLine,false);
					}
					else
						displayManager.expandAllFolds();
				}
				else
					displayManager.expandFold(newCaretLine,false);
			}

			if(bracketLine != -1)
			{
				invalidateLineRange(bracketLine,caretLine);
				bracketLine = bracketPosition = -1;
			}

			if(caretLine == newCaretLine)
			{
				if(caretScreenLine != -1)
					invalidateScreenLineRange(caretScreenLine,caretScreenLine);
			}
			else
			{
				int newCaretScreenLine = chunkCache.getScreenLineOfOffset(newCaretLine,
					newCaret - buffer.getLineStartOffset(newCaretLine));
				if(caretScreenLine == -1)
					invalidateScreenLineRange(newCaretScreenLine,newCaretScreenLine);
				else
					invalidateScreenLineRange(caretScreenLine,newCaretScreenLine);
				caretScreenLine = newCaretScreenLine;
			}

			caret = newCaret;
			caretLine = newCaretLine;

			if(scrollMode == NORMAL_SCROLL)
				finishCaretUpdate(false,true);
			else if(scrollMode == ELECTRIC_SCROLL)
				finishCaretUpdate(true,true);
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

	
	
	public final int getBracketPosition()
	{
		return bracketPosition;
	} 

	
	
	public final int getBracketLine()
	{
		return bracketLine;
	} 

	
	
	public void goToNextBracket(boolean select)
	{
		String text = getText(caret,buffer.getLength() - caret - 1);

		int newCaret = -1;

loop:		for(int i = 0; i < text.length(); i++)
		{
			switch(text.charAt(i))
			{
			case ')': case ']': case '}':
				newCaret = caret + i + 1;
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

		if(select && caret == buffer.getLength())
		{
			if(rectangularSelectionMode || s instanceof Selection.Rect)
			{
				if(caret == s.start)
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
			if(rectangularSelectionMode || s instanceof Selection.Rect)
			{
				if(caret == s.start)
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
		boolean rectSelect = (s == null ? rectangularSelectionMode
			: s instanceof Selection.Rect);
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

		setMagicCaretPosition(magic);
	} 

	
	
	public void goToNextMarker(boolean select)
	{
		Vector markers = buffer.getMarkers();
		if(markers.size() == 0)
		{
			getToolkit().beep();
			return;
		}

		Marker marker = null;

		for(int i = 0; i < markers.size(); i++)
		{
			Marker _marker = (Marker)markers.get(i);
			if(_marker.getPosition() > caret)
			{
				marker = _marker;
				break;
			}
		}

		if(marker == null)
			marker = (Marker)markers.get(0);

		if(select)
			extendSelection(caret,marker.getPosition());
		else if(!multi)
			selectNone();
		moveCaretPosition(marker.getPosition());
	} 

	
	
	public void goToNextPage(boolean select)
	{
		scrollToCaret(false);
		int magic = getMagicCaretPosition();
		int caretScreenLine = getScreenLineOfOffset(caret);

		scrollDownPage();

		int newCaret = xToScreenLineOffset(caretScreenLine,magic,true);

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

	
	
	public void goToNextWord(boolean select, boolean stdNextPrevWord)
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
			newCaret = TextUtilities.findWordEnd(lineText,newCaret + 1,
				noWordSep);
			if(stdNextPrevWord)
			{
				while((newCaret < lineText.length()) && Character.isWhitespace(lineText.charAt(newCaret)))
					newCaret = TextUtilities.findWordEnd(lineText,newCaret + 1,
						noWordSep);
			}

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
		boolean rectSelect = (s == null ? rectangularSelectionMode
			: s instanceof Selection.Rect);
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

		setMagicCaretPosition(magic);
	} 

	
	
	public void goToPrevMarker(boolean select)
	{
		Vector markers = buffer.getMarkers();
		if(markers.size() == 0)
		{
			getToolkit().beep();
			return;
		}

		Marker marker = null;
		for(int i = markers.size() - 1; i >= 0; i--)
		{
			Marker _marker = (Marker)markers.elementAt(i);
			if(_marker.getPosition() < caret)
			{
				marker = _marker;
				break;
			}
		}

		if(marker == null)
			marker = (Marker)markers.get(markers.size() - 1);

		if(select)
			extendSelection(caret,marker.getPosition());
		else if(!multi)
			selectNone();
		moveCaretPosition(marker.getPosition());
	} 

	
	
	public void goToPrevPage(boolean select)
	{
		scrollToCaret(false);
		int magic = getMagicCaretPosition();
		int caretScreenLine = getScreenLineOfOffset(caret);

		scrollUpPage();

		int newCaret = xToScreenLineOffset(caretScreenLine,magic,true);

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

	
	
	public void goToPrevWord(boolean select, boolean stdNextPrevWord)
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
			newCaret = TextUtilities.findWordStart(lineText,newCaret - 1,
				noWordSep);
			if(stdNextPrevWord)
			{
				while((newCaret > 0) && Character.isWhitespace(lineText.charAt(newCaret)))
					newCaret = TextUtilities.findWordStart(lineText,newCaret - 1,
						noWordSep);
			}
			
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
		Macros.Recorder recorder = view.getMacroRecorder();

		switch(view.getInputHandler().getLastActionCount())
		{
		case 1:
			if(recorder != null)
				recorder.record("textArea.goToStartOfWhiteSpace(" + select + ");");

			goToStartOfWhiteSpace(select);
			break;
		case 2:
			if(recorder != null)
				recorder.record("textArea.goToStartOfLine(" + select + ");");

			goToStartOfLine(select);
			break;
		default: 
			if(recorder != null)
				recorder.record("textArea.goToFirstVisibleLine(" + select + ");");

			goToFirstVisibleLine(select);
			break;
		}
	} 

	
	
	public void smartEnd(boolean select)
	{
		Macros.Recorder recorder = view.getMacroRecorder();

		switch(view.getInputHandler().getLastActionCount())
		{
		case 1:
			if(recorder != null)
				recorder.record("textArea.goToEndOfWhiteSpace(" + select + ");");

			goToEndOfWhiteSpace(select);
			break;
		case 2:
			if(recorder != null)
				recorder.record("textArea.goToEndOfLine(" + select + ");");

			goToEndOfLine(select);
			break;
		default: 
			if(recorder != null)
				recorder.record("textArea.goToLastVisibleLine(" + select + ");");
			goToLastVisibleLine(select);
			break;
		}
	} 

	
	
	public void goToStartOfLine(boolean select)
	{
		Selection s = getSelectionAtOffset(caret);
		int line = (select || s == null
			? caretLine : s.startLine);
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
		int line = (select || s == null
			? caretLine : s.endLine);
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
			firstIndent = MiscUtilities.getLeadingWhiteSpace(getLineText(line));
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
			lastIndent = getLineLength(line) - MiscUtilities.getTrailingWhiteSpace(getLineText(line));
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
			lastVisible = getScreenLineEndOffset(visibleLines
				- electricScroll - 1) - 1;
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

	
	
	public void showGoToLineDialog()
	{
		String line = GUIUtilities.input(view,"goto-line",null);
		if(line == null)
			return;

		try
		{
			int lineNumber = Integer.parseInt(line) - 1;
			setCaretPosition(getLineStartOffset(lineNumber));
		}
		catch(Exception e)
		{
			getToolkit().beep();
		}
	} 

	

	

	
	
	public void userInput(char ch)
	{
		if(!isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(ch == ' ' && Abbrevs.getExpandOnInput()
			&& Abbrevs.expandAbbrev(view,false))
			return;
		else if(ch == '\t')
		{
			if(selection.size() == 1)
			{
				Selection sel = (Selection)selection.elementAt(0);
				if(sel.startLine == sel.endLine
					&& (sel.start != buffer.getLineStartOffset(sel.startLine)
					|| sel.end != buffer.getLineEndOffset(sel.startLine) - 1))
				{
					insertTab();
				}
				else
					shiftIndentRight();
			}
			else if(selection.size() != 0)
				shiftIndentRight();
			else
				insertTab();
			return;
		}
		else
		{
			boolean indent;

			
			String indentOpenBrackets = (String)buffer
				.getProperty("indentOpenBrackets");
			String indentCloseBrackets = (String)buffer
				.getProperty("indentCloseBrackets");
			if((indentCloseBrackets != null
				&& indentCloseBrackets.indexOf(ch) != -1)
				|| (indentOpenBrackets != null
				&& indentOpenBrackets.indexOf(ch) != -1))
			{
				indent = true;
			}
			else
			{
				indent = false;
			}

			String str = String.valueOf(ch);
			Selection[] selection = getSelection();
			if(selection.length != 0)
			{
				for(int i = 0; i < selection.length; i++)
				{
					Selection s = selection[i];
					setSelectedText(s,str);
					
				}
				return;
			}

			if(ch == ' ')
			{
				if(doWordWrap(true))
					return;
			}
			else
				doWordWrap(false);

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
					buffer.indentLine(caretLine,false,true);
			}
			finally
			{
				if(overwrite || indent)
					buffer.endCompoundEdit();
			}

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
		if(view.getStatus() != null)
			view.getStatus().updateMiscStatus();
	} 

	
	
	public final void toggleOverwriteEnabled()
	{
		setOverwriteEnabled(!overwrite);
		if(view.getStatus() != null)
		{
			view.getStatus().setMessageAndClear(
				jEdit.getProperty("view.status.overwrite-changed",
				new Integer[] { new Integer(overwrite ? 1 : 0) }));
		}
	} 

	
	
	public void backspace()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(selection.size() != 0)
			setSelectedText(null);
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

	
	
	public void backspaceWord()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(selection.size() != 0)
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
			_caret = TextUtilities.findWordStart(lineText,_caret-1,noWordSep);
		}

		buffer.remove(_caret + lineStart,
			caret - (_caret + lineStart));
	} 

	
	
	public void delete()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(selection.size() != 0)
			setSelectedText(null);
		else
		{
			if(caret == buffer.getLength())
			{
				getToolkit().beep();
				return;
			}

			buffer.remove(caret,1);
		}
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

		int start = getLineStartOffset(caretLine);
		int end = getLineEndOffset(caretLine);

		int x = chunkCache.subregionOffsetToX(caretLine,caret - start);

		
		
		try
		{
			if(end > buffer.getLength())
			{
				if(start != 0)
					start--;
				end--;
			}
			buffer.beginCompoundEdit();
			buffer.remove(start,end - start);
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
			setCaretPosition(start + offset);
		}
	} 

	
	
	public void deleteParagraph()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		int start = 0, end = buffer.getLength();

loop:		for(int i = caretLine - 1; i >= 0; i--)
		{
			
			

			getLineText(i,lineSegment);

			for(int j = 0; j < lineSegment.count; j++)
			{
				switch(lineSegment.array[lineSegment.offset + j])
				{
				case ' ':
				case '\t':
					break;
				default:
					continue loop;
				}
			}

			start = getLineStartOffset(i);
			break loop;
		}

loop:		for(int i = caretLine + 1; i < getLineCount(); i++)
		{
			
			

			getLineText(i,lineSegment);

			for(int j = 0; j < lineSegment.count; j++)
			{
				switch(lineSegment.array[lineSegment.offset + j])
				{
				case ' ':
				case '\t':
					break;
				default:
					continue loop;
				}
			}

			end = getLineEndOffset(i) - 1;
			break loop;
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
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(selection.size() != 0)
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
				_caret+1,noWordSep);
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
		if(view.getStatus() != null)
		{
			view.getStatus().setMessageAndClear(
				jEdit.getProperty("view.status.multi-changed",
				new Integer[] { new Integer(multi ? 1 : 0) }));
		}
	} 

	
	
	public final void setMultipleSelectionEnabled(boolean multi)
	{
		this.multi = multi;
		if(view.getStatus() != null)
			view.getStatus().updateMiscStatus();
		painter.repaint();
	} 

	
	
	public final boolean isRectangularSelectionEnabled()
	{
		return rectangularSelectionMode;
	} 

	
	
	public final void toggleRectangularSelectionEnabled()
	{
		setRectangularSelectionEnabled(!rectangularSelectionMode);
		if(view.getStatus() != null)
		{
			view.getStatus().setMessageAndClear(
				jEdit.getProperty("view.status.rect-select-changed",
				new Integer[] { new Integer(rectangularSelectionMode ? 1 : 0) }));
		}
	} 

	
	
	public final void setRectangularSelectionEnabled(boolean rectangularSelectionMode)
	{
		this.rectangularSelectionMode = rectangularSelectionMode;
		if(view.getStatus() != null)
			view.getStatus().updateMiscStatus();
		painter.repaint();
	} 

	

	

	
	
	public void goToMarker(char shortcut, boolean select)
	{
		Marker marker = buffer.getMarker(shortcut);
		if(marker == null)
		{
			getToolkit().beep();
			return;
		}

		int pos = marker.getPosition();

		if(select)
			extendSelection(caret,pos);
		else if(!multi)
			selectNone();
		moveCaretPosition(pos);
	} 

	
	
	public void addMarker()
	{
		
		Selection[] selection = getSelection();
		for(int i = 0; i < selection.length; i++)
		{
			Selection s = selection[i];
			if(s.startLine != s.endLine)
			{
				if(s.startLine != caretLine)
					buffer.addMarker('\0',s.start);
			}

			if(s.endLine != caretLine)
				buffer.addMarker('\0',s.end);
		}

		
		buffer.addOrRemoveMarker('\0',caret);
	} 

	
	
	public void swapMarkerAndCaret(char shortcut)
	{
		Marker marker = buffer.getMarker(shortcut);
		if(marker == null)
		{
			getToolkit().beep();
			return;
		}

		int caret = getCaretPosition();

		setCaretPosition(marker.getPosition());
		buffer.addMarker(shortcut,caret);
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
		int x = chunkCache.subregionOffsetToX(caretLine,
			caret - getLineStartOffset(caretLine));

		displayManager.collapseFold(caretLine);

		if(displayManager.isLineVisible(caretLine))
			return;

		int line = displayManager.getPrevVisibleLine(caretLine);

		if(!multi)
			selectNone();
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
		if(selection.size() != 1)
		{
			getToolkit().beep();
			return;
		}

		Selection sel = (Selection)selection.elementAt(0);
		displayManager.narrow(sel.getStartLine(),sel.getEndLine());

		selectNone();
	} 

	
	
	public void addExplicitFold()
	{
		if(!buffer.getStringProperty("folding").equals("explicit"))
		{
			GUIUtilities.error(view,"folding-not-explicit",null);
			return;
		}

		
		
		String lineComment = buffer.getContextSensitiveProperty(caret,"lineComment");
		String commentStart = buffer.getContextSensitiveProperty(caret,"commentStart");
		String commentEnd = buffer.getContextSensitiveProperty(caret,"commentEnd");

		String start, end;
		if(lineComment != null)
		{
			start = lineComment + "{{{ \n";
			end = lineComment + "}}}";
		}
		else if(commentStart != null && commentEnd != null)
		{
			start = commentStart + "{{{  " + commentEnd + "\n";
			end = commentStart + "}}}" + commentEnd;
		}
		else
		{
			start = "{{{ \n";
			end = "}}}";
		}

		try
		{
			buffer.beginCompoundEdit();

			if(selection.size() == 0)
			{
				String line = buffer.getLineText(caretLine);
				String whitespace = line.substring(0,
					MiscUtilities.getLeadingWhiteSpace(line));
				int loc = caret + start.length() - 1;
				start = start + whitespace;
				buffer.insert(caret,start);
				
				buffer.insert(caret,end);
				moveCaretPosition(loc,false);
			}
			else
			{
				int loc = -1;

				for(int i = 0; i < selection.size(); i++)
				{
					Selection s = (Selection)selection.elementAt(i);
					String line = buffer.getLineText(s.startLine);
					String whitespace = line.substring(0,
						MiscUtilities.getLeadingWhiteSpace(line));
					loc = s.start + start.length() - 1;
					buffer.insert(s.start,start + whitespace);
					buffer.insert(s.end," " + end);
				}

				setCaretPosition(loc,false);
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

		comment = comment + ' ';

		buffer.beginCompoundEdit();

		int[] lines = getSelectedLines();

		try
		{
			for(int i = 0; i < lines.length; i++)
			{
				String text = getLineText(lines[i]);
				buffer.insert(getLineStartOffset(lines[i])
					+ MiscUtilities.getLeadingWhiteSpace(text),
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

		commentStart = commentStart + ' ';
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

	
	
	public void formatParagraph()
	{
		if(!buffer.isEditable())
		{
			getToolkit().beep();
			return;
		}

		if(maxLineLen <= 0)
		{
			getToolkit().beep();
			return;
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

loop:			for(int i = lineNo - 1; i >= 0; i--)
			{
				getLineText(i,lineSegment);

				for(int j = 0; j < lineSegment.count; j++)
				{
					switch(lineSegment.array[lineSegment.offset + j])
					{
					case ' ':
					case '\t':
						break;
					default:
						continue loop;
					}
				}

				start = getLineEndOffset(i);
				break loop;
			}

loop:			for(int i = lineNo + 1; i < getLineCount(); i++)
			{
				getLineText(i,lineSegment);

				for(int j = 0; j < lineSegment.count; j++)
				{
					switch(lineSegment.array[lineSegment.offset + j])
					{
					case ' ':
					case '\t':
						break;
					default:
						continue loop;
					}
				}

				end = getLineStartOffset(i) - 1;
				break loop;
			}

			try
			{
				buffer.beginCompoundEdit();

				String text = buffer.getText(start,end - start);
				buffer.remove(start,end - start);
				buffer.insert(start,TextUtilities.format(
					text,maxLineLen,buffer.getTabSize()));
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
		Selection[] selection = getSelection();

		if(!buffer.isEditable() || selection.length == 0)
                {
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
	} 

	
	
	public void toLowerCase()
	{
		Selection[] selection = getSelection();

		if(!buffer.isEditable() || selection.length == 0)
                {
                	getToolkit().beep();
                	return;
                }

		buffer.beginCompoundEdit();

		for(int i = 0; i < selection.length; i++)
		{
			Selection s = selection[i];
			setSelectedText(s,getSelectedText(s).toLowerCase());
		}

		buffer.endCompoundEdit();
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
				buffer.indentLine(caretLine,true,false);
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

		if(selection.size() == 0)
		{
			
			String text = buffer.getLineText(caretLine);
			int start = buffer.getLineStartOffset(caretLine);
			int whiteSpace = MiscUtilities.getLeadingWhiteSpace(text);

			if(caret - start <= whiteSpace
				&& buffer.indentLine(caretLine,true,false))
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
		int end = getLineEndOffset(caretLine);
		if(end > buffer.getLength())
		{
			getToolkit().beep();
			return;
		}
		buffer.remove(end - 1,MiscUtilities.getLeadingWhiteSpace(
			buffer.getLineText(caretLine + 1)) + 1);

		setCaretPosition(end - 1);
	} 

	
	
	public void showWordCountDialog()
	{
		String selection = getSelectedText();
		if(selection != null)
		{
			doWordCount(view,selection);
			return;
		}

		doWordCount(view,buffer.getText(0,buffer.getLength()));
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

		if(!bufferHandlerInstalled)
		{
			bufferHandlerInstalled = true;
			buffer.addBufferChangeListener(bufferHandler);
		}

		recalculateVisibleLines();
		if(buffer.isLoaded())
			recalculateLastPhysicalLine();
		propertiesChanged();
	} 

	
	
	public void removeNotify()
	{
		super.removeNotify();

		ToolTipManager.sharedInstance().unregisterComponent(painter);
		ToolTipManager.sharedInstance().unregisterComponent(gutter);

		if(focusedComponent == this)
			focusedComponent = null;

		if(bufferHandlerInstalled)
		{
			buffer.removeBufferChangeListener(bufferHandler);
			bufferHandlerInstalled = false;
		}
	} 

	
	
	public boolean hasFocus()
	{
		Component c = this;
		while(!(c instanceof Window))
		{
			if(c == null)
				return false;
			c = c.getParent();
		}

		Component focusOwner = ((Window)c).getFocusOwner();
		boolean hasFocus = (focusOwner == this);
		if(hasFocus)
			focusedComponent = this;
		return hasFocus;
	} 

	
	
	public void grabFocus()
	{
		super.grabFocus();
		
		hasFocus();
	} 

	
	
	public boolean getFocusTraversalKeysEnabled()
	{
		return false;
	} 

	

	
	long time;

	public void processKeyEvent(KeyEvent evt)
	{
		time = System.currentTimeMillis();
		evt = KeyEventWorkaround.processKeyEvent(evt);
		if(evt == null)
			return;

		
		if(view.isClosed())
			return;

		InputHandler inputHandler = view.getInputHandler();
		KeyListener keyEventInterceptor = view.getKeyEventInterceptor();
		switch(evt.getID())
		{
		case KeyEvent.KEY_TYPED:
			
			if(keyEventInterceptor != null)
				keyEventInterceptor.keyTyped(evt);
			else
				inputHandler.keyTyped(evt);
			break;
		case KeyEvent.KEY_PRESSED:
			if(keyEventInterceptor != null)
				keyEventInterceptor.keyPressed(evt);
			else
				inputHandler.keyPressed(evt);
			break;
		case KeyEvent.KEY_RELEASED:
			if(keyEventInterceptor != null)
				keyEventInterceptor.keyReleased(evt);
			else
				inputHandler.keyReleased(evt);
			break;
		}

		if(!evt.isConsumed())
			super.processKeyEvent(evt);
	} 

	

	
	
	public void propertiesChanged()
	{
		if(buffer == null)
			return;

		int _tabSize = buffer.getTabSize();
		char[] foo = new char[_tabSize];
		for(int i = 0; i < foo.length; i++)
		{
			foo[i] = ' ';
		}

		tabSize = (float)painter.getFont().getStringBounds(foo,0,_tabSize,
			painter.getFontRenderContext()).getWidth();

		charWidth = (int)Math.round(painter.getFont().getStringBounds(foo,0,1,
			painter.getFontRenderContext()).getWidth());

		String wrap = buffer.getStringProperty("wrap");
		hardWrap = wrap.equals("hard");

		maxLineLen = buffer.getIntegerProperty("maxLineLen",0);

		maxHorizontalScrollWidth = 0;

		if(displayManager != null && !bufferChanging)
			displayManager.updateWrapSettings();

		chunkCache.invalidateAll();
		gutter.repaint();
		painter.repaint();
	} 

	

	
	
	public final int getSelectionStart()
	{
		if(selection.size() != 1)
			return caret;

		return ((Selection)selection.elementAt(0)).getStart();
	} 

	
	
	public int getSelectionStart(int line)
	{
		if(selection.size() != 1)
			return caret;

		return ((Selection)selection.elementAt(0)).getStart(
			buffer,line);
	} 

	
	
	public final int getSelectionStartLine()
	{
		if(selection.size() != 1)
			return caret;

		return ((Selection)selection.elementAt(0)).getStartLine();
	} 

	
	
	public final void setSelectionStart(int selectionStart)
	{
		select(selectionStart,getSelectionEnd(),true);
	} 

	
	
	public final int getSelectionEnd()
	{
		if(selection.size() != 1)
			return caret;

		return ((Selection)selection.elementAt(0)).getEnd();
	} 

	
	
	public int getSelectionEnd(int line)
	{
		if(selection.size() != 1)
			return caret;

		return ((Selection)selection.elementAt(0)).getEnd(
			buffer,line);
	} 

	
	
	public final int getSelectionEndLine()
	{
		if(selection.size() != 1)
			return caret;

		return ((Selection)selection.elementAt(0)).getEndLine();
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
		if(selection.size() != 1)
			return caretLine;

		Selection s = (Selection)selection.elementAt(0);
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
		if(s == null)
			return false;
		else
			return (s instanceof Selection.Rect);
	} 

	

	

	
	Segment lineSegment;
	MouseHandler mouseHandler;
	ChunkCache chunkCache;
	DisplayManager displayManager;
	boolean bufferChanging;

	int maxHorizontalScrollWidth;

	boolean hardWrap;
	float tabSize;
	int charWidth;
	int maxLineLen;

	boolean scrollBarsInitialized;

	
	
	
	
	
	
	boolean queuedScrollTo;
	boolean queuedScrollToElectric;
	boolean queuedFireCaretEvent;
	ArrayList runnables;

	
	
	Vector selection;

	
	Point returnValue;

	boolean lastLinePartial;
	

	
	
	final boolean isCaretVisible()
	{
		return blink && hasFocus();
	} 

	
	
	final boolean isBracketHighlightVisible()
	{
		return bracketLine != -1
			&& hasFocus()
			&& displayManager.isLineVisible(bracketLine)
			&& displayManager.isLineVisible(caretLine);
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
		}
	} 

	
	void recalculateVisibleLines()
	{
		if(painter == null)
			return;
		int height = painter.getHeight();
		int lineHeight = painter.getFontMetrics().getHeight();
		visibleLines = height / lineHeight;
		lastLinePartial = (height % lineHeight != 0);
		if(lastLinePartial)
			visibleLines++;

		chunkCache.recalculateVisibleLines();
		maxHorizontalScrollWidth = 0;

		
		if(displayManager != null && buffer != null && buffer.isLoaded())
			setFirstLine(getFirstLine());

		updateScrollBars();
	} 

	
	void foldStructureChanged()
	{
		chunkCache.invalidateAll();
		recalculateLastPhysicalLine();
		repaint();
	} 

	
	
	void updateScrollBars()
	{
		if(buffer == null)
			return;

		if(Debug.SCROLL_DEBUG)
			Log.log(Log.DEBUG,this,"updateScrollBars()");

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

		int width = painter.getWidth();
		if(horizontal != null && width != 0)
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"Horizontal ok");
			maxHorizontalScrollWidth = 0;
			painter.repaint();

			horizontal.setUnitIncrement(painter.getFontMetrics()
				.charWidth('w'));
			horizontal.setBlockIncrement(width / 2);
		}
	} 

	

	

	
	private static String CENTER = "center";
	private static String RIGHT = "right";
	private static String LEFT = "left";
	private static String BOTTOM = "bottom";

	private static Timer caretTimer;
	private static Timer bracketTimer;
	private static JEditTextArea focusedComponent;
	

	
	private View view;
	private Gutter gutter;
	private TextAreaPainter painter;

	private JPopupMenu popup;

	private EventListenerList listenerList;
	private MutableCaretEvent caretEvent;

	private boolean caretBlinks;
	private boolean blink;

	private int physLastLine;
	private int screenLastLine;

	private int visibleLines;
	private int electricScroll;

	private int horizontalOffset;

	private boolean quickCopy;

	
	private Box verticalBox;
	private JScrollBar vertical;
	private JScrollBar horizontal;

	private Buffer buffer;
	private BufferChangeHandler bufferHandler;
	private boolean bufferHandlerInstalled;

	private int caret;
	private int caretLine;
	private int caretScreenLine;

	private int bracketPosition;
	private int bracketLine;

	private int magicCaret;

	private boolean multi;
	private boolean overwrite;
	private boolean rectangularSelectionMode;

	private boolean delayedScrollTo;
	

	
	private void _addToSelection(Selection addMe)
	{
		if(addMe.start > addMe.end)
		{
			throw new IllegalArgumentException(addMe.start
				+ " > " + addMe.end);
		}
		else if(addMe.start == addMe.end)
		{
			if(addMe instanceof Selection.Range)
				return;
			else if(addMe instanceof Selection.Rect)
			{
				if(((Selection.Rect)addMe).extraEndVirt == 0)
					return;
			}
		}

		for(int i = 0; i < selection.size(); i++)
		{
			
			
			Selection s = (Selection)selection.elementAt(i);
			if(s.overlaps(addMe))
			{
				addMe.start = Math.min(s.start,addMe.start);
				addMe.end = Math.max(s.end,addMe.end);

				selection.removeElement(s);
				i--;
			}
		}

		addMe.startLine = getLineOfOffset(addMe.start);
		addMe.endLine = getLineOfOffset(addMe.end);

		boolean added = false;

		for(int i = 0; i < selection.size(); i++)
		{
			Selection s = (Selection)selection.elementAt(i);
			if(addMe.start < s.start)
			{
				selection.insertElementAt(addMe,i);
				added = true;
				break;
			}
		}

		if(!added)
			selection.addElement(addMe);

		invalidateLineRange(addMe.startLine,addMe.endLine);
	} 

	
	
	private void finishCaretUpdate(boolean doElectricScroll,
		boolean fireCaretEvent)
	{
		this.queuedScrollToElectric |= doElectricScroll;
		this.queuedFireCaretEvent |= fireCaretEvent;

		if(queuedScrollTo)
			return;

		Runnable r = new Runnable()
		{
			public void run()
			{
				
				
				blink = true;
				caretTimer.restart();

				scrollToCaret(queuedScrollToElectric);
				updateBracketHighlightWithDelay();
				if(queuedFireCaretEvent)
					fireCaretEvent();
				queuedScrollTo = queuedScrollToElectric
					= queuedFireCaretEvent = false;
			}
		};

		if(buffer.isTransactionInProgress())
		{
			queuedScrollTo = true;
			runnables.add(r);
		}
		else
			r.run();
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

			setSelectedText(MiscUtilities.createWhiteSpace(
				tabSize - pos,0));
		}
		else
			setSelectedText("\t");
	} 

	
	private boolean doWordWrap(boolean spaceInserted)
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

		boolean returnValue = false;

		int tabSize = buffer.getTabSize();

		String wordBreakChars = buffer.getStringProperty("wordBreakChars");

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
					lastWordOffset = i;
					lastWasSpace = true;
				}
			}
			else if(ch == ' ')
			{
				logicalLength++;
				if(!lastWasSpace && logicalLength <= maxLineLen)
				{
					lastWordOffset = i;
					lastWasSpace = true;
				}
			}
			else if(wordBreakChars != null && wordBreakChars.indexOf(ch) != -1)
			{
				logicalLength++;
				if(!lastWasSpace && logicalLength <= maxLineLen)
				{
					lastWordOffset = i;
					lastWasSpace = true;
				}
			}
			else
			{
				logicalLength++;
				lastWasSpace = false;
			}

			int insertNewLineAt;
			if(spaceInserted && logicalLength == maxLineLen
				&& i == caretPos - 1)
			{
				insertNewLineAt = caretPos;
				returnValue = true;
			}
			else if(logicalLength >= maxLineLen && lastWordOffset != -1)
				insertNewLineAt = lastWordOffset;
			else
				continue;

			try
			{
				buffer.beginCompoundEdit();
				buffer.insert(start + insertNewLineAt,"\n");
				
				
				buffer.indentLine(caretLine,true,true);
			}
			finally
			{
				buffer.endCompoundEdit();
			}

			
			return returnValue;
		}

		return false;
	} 

	
	private void doWordCount(View view, String text)
	{
		char[] chars = text.toCharArray();
		int characters = chars.length;
		int words;
		if(characters == 0)
			words = 0;
		else
			words = 1;
		int lines = 1;
		boolean word = false;
		for(int i = 0; i < chars.length; i++)
		{
			switch(chars[i])
			{
			case '\r': case '\n':
				lines++;
			case ' ': case '\t':
				if(word)
				{
					words++;
					word = false;
				}
				break;
			default:
				word = true;
				break;
			}
		}
		Object[] args = { new Integer(characters), new Integer(words),
			new Integer(lines) };
		GUIUtilities.message(view,"wordcount",args);
	} 

	
	private void updateBracketHighlightWithDelay()
	{
		bracketTimer.stop();
		bracketTimer.start();
	} 

	
	private void updateBracketHighlight()
	{
		if(!painter.isBracketHighlightEnabled()
			&& !gutter.isBracketHighlightEnabled())
			return;

		int offset = caret - getLineStartOffset(caretLine);

		if(offset != 0)
		{
			int bracketOffset = TextUtilities.findMatchingBracket(
				buffer,caretLine,offset - 1);
			if(bracketOffset != -1)
			{
				bracketLine = getLineOfOffset(bracketOffset);
				bracketPosition = bracketOffset
					- getLineStartOffset(bracketLine);
				invalidateLineRange(bracketLine,caretLine);

				if(chunkCache.getScreenLineOfOffset(bracketLine,bracketPosition) == -1)
					showBracketStatusMessage(bracketLine < caretLine);
				return;
			}
		}

		bracketLine = bracketPosition = -1;
	} 

	
	private void showBracketStatusMessage(boolean backward)
	{
		String text = buffer.getLineText(bracketLine).trim();
		if(backward && bracketLine != 0 && text.length() == 1)
		{
			switch(text.charAt(0))
			{
			case '{': case '}':
			case '[': case ']':
			case '(': case ')':
				text = buffer.getLineText(bracketLine - 1).trim()
					+ " " + text;
				break;
			}
		}

		
		text = text.replace('\t',' ');

		view.getStatus().setMessageAndClear(jEdit.getProperty(
			"view.status.bracket",new Object[] { 
			new Integer(bracketLine), text }));
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
		int extraStartVirt;
		int extraEndVirt;
		int newCaret;

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
			RectParams returnValue;

			boolean bias;
			if(s == null)
				bias = (newCaret < caret);
			else if(s.start == caret)
				bias = (newCaret <= s.end);
			else if(s.end == caret)
				bias = (newCaret <= s.start);
			else
				bias = false;

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

	

	

	
	static class TextAreaBorder extends AbstractBorder
	{
		private static final Insets insets = new Insets(1, 1, 2, 2);

		
		public void paintBorder(Component c, Graphics g, int x, int y,
			int width, int height)
		{
			g.translate(x,y);

			g.setColor(MetalLookAndFeel.getControlDarkShadow());
			g.drawRect(0,0,width-2,height-2);

			g.setColor(MetalLookAndFeel.getControlHighlight());
			g.drawLine(width-1,1,width-1,height-1);
			g.drawLine(1,height-1,width-1,height-1);

			g.setColor(MetalLookAndFeel.getControl());
			g.drawLine(width-2,2,width-2,2);
			g.drawLine(1,height-2,1,height-2);

			g.translate(-x,-y);
		} 

		
		public Insets getBorderInsets(Component c)
		{
			return new Insets(1,1,2,2);
		} 
	} 

	
	class ScrollLayout implements LayoutManager
	{
		
		public void addLayoutComponent(String name, Component comp)
		{
			if(name.equals(CENTER))
				center = comp;
			else if(name.equals(RIGHT))
				right = comp;
			else if(name.equals(LEFT))
				left = comp;
			else if(name.equals(BOTTOM))
				bottom = comp;
		} 

		
		public void removeLayoutComponent(Component comp)
		{
			if(center == comp)
				center = null;
			else if(right == comp)
				right = null;
			else if(left == comp)
				left = null;
			else if(bottom == comp)
				bottom = null;
		} 

		
		public Dimension preferredLayoutSize(Container parent)
		{
			Dimension dim = new Dimension();
			Border border = getBorder();
			Insets insets;
			if(border == null)
				insets = new Insets(0,0,0,0);
			else
			{
				insets = getBorder().getBorderInsets(
					JEditTextArea.this);
			}

			dim.width = insets.left + insets.right;
			dim.height = insets.top + insets.bottom;

			Dimension leftPref = left.getPreferredSize();
			dim.width += leftPref.width;
			Dimension centerPref = center.getPreferredSize();
			dim.width += centerPref.width;
			dim.height += centerPref.height;
			Dimension rightPref = right.getPreferredSize();
			dim.width += rightPref.width;
			Dimension bottomPref = bottom.getPreferredSize();
			dim.height += bottomPref.height;

			return dim;
		} 

		
		public Dimension minimumLayoutSize(Container parent)
		{
			Dimension dim = new Dimension();
			Border border = getBorder();
			Insets insets;
			if(border == null)
				insets = new Insets(0,0,0,0);
			else
			{
				insets = getBorder().getBorderInsets(
					JEditTextArea.this);
			}

			dim.width = insets.left + insets.right;
			dim.height = insets.top + insets.bottom;

			Dimension leftPref = left.getMinimumSize();
			dim.width += leftPref.width;
			Dimension centerPref = center.getMinimumSize();
			dim.width += centerPref.width; 
			dim.height += centerPref.height;
			Dimension rightPref = right.getMinimumSize();
			dim.width += rightPref.width;
			Dimension bottomPref = bottom.getMinimumSize();
			dim.height += bottomPref.height;

			return dim;
		} 

		
		public void layoutContainer(Container parent)
		{
			Dimension size = parent.getSize();
			Border border = getBorder();
			Insets insets;
			if(border == null)
				insets = new Insets(0,0,0,0);
			else
			{
				insets = getBorder().getBorderInsets(
					JEditTextArea.this);
			}

			int itop = insets.top;
			int ileft = insets.left;
			int ibottom = insets.bottom;
			int iright = insets.right;

			int rightWidth = right.getPreferredSize().width;
			int leftWidth = left.getPreferredSize().width;
			int bottomHeight = bottom.getPreferredSize().height;
			int centerWidth = Math.max(0,size.width - leftWidth
				- rightWidth - ileft - iright);
			int centerHeight = Math.max(0,size.height
				- bottomHeight - itop - ibottom);

			left.setBounds(
				ileft,
				itop,
				leftWidth,
				centerHeight);

			center.setBounds(
				ileft + leftWidth,
				itop,
				centerWidth,
				centerHeight);

			right.setBounds(
				ileft + leftWidth + centerWidth,
				itop,
				rightWidth,
				centerHeight);

			bottom.setBounds(
				ileft,
				itop + centerHeight,
				Math.max(0,size.width - rightWidth - ileft - iright),
				bottomHeight);
		} 

		Component center;
		Component left;
		Component right;
		Component bottom;
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
			super(JEditTextArea.this);
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
		
		public void adjustmentValueChanged(final AdjustmentEvent evt)
		{
			if(!scrollBarsInitialized)
				return;

			if(evt.getAdjustable() == vertical)
				setFirstLine(vertical.getValue());
			else
				setHorizontalOffset(-horizontal.getValue());
		} 
	} 

	
	
	class BufferChangeHandler extends BufferChangeAdapter
	{
		boolean delayedUpdate;
		boolean delayedMultilineUpdate;
		int delayedRepaintStart;
		int delayedRepaintEnd;

		
		public void foldLevelChanged(Buffer buffer, int start, int end)
		{
			if(!bufferChanging && end != 0
				&& buffer.isLoaded())
			{
				invalidateLineRange(start - 1,
					getLastPhysicalLine());
			}
		} 

		
		public void contentInserted(Buffer buffer, int startLine, int start,
			int numLines, int length)
		{
			if(!buffer.isLoaded())
				return;

			if(numLines != 0)
				delayedMultilineUpdate = true;

			int endLine = startLine + numLines;

			delayedRepaint(startLine,endLine);

			
			for(int i = 0; i < selection.size(); i++)
			{
				Selection s = (Selection)selection.elementAt(i);

				if(s.contentInserted(buffer,startLine,start,
					numLines,length))
				{
					delayedRepaint(s.startLine,s.endLine);
				}
			} 

			
			
			if(!buffer.isTransactionInProgress())
				transactionComplete(buffer);

			if(caret >= start)
			{
				int scrollMode = (focusedComponent == JEditTextArea.this
					? ELECTRIC_SCROLL : NO_SCROLL);
				moveCaretPosition(caret + length,scrollMode);
			}
			else
			{
				int scrollMode = (focusedComponent == JEditTextArea.this
					? NORMAL_SCROLL : NO_SCROLL);
				moveCaretPosition(caret,scrollMode);
			}
		}
		

		
		public void contentRemoved(Buffer buffer, int startLine, int start,
			int numLines, int length)
		{
			if(!buffer.isLoaded())
				return;

			if(numLines != 0)
				delayedMultilineUpdate = true;

			delayedRepaint(startLine,startLine);

			
			for(int i = 0; i < selection.size(); i++)
			{
				Selection s = (Selection)selection.elementAt(i);

				int oldStartLine = s.startLine;
				int oldEndLine = s.endLine;
				boolean changed = s.contentRemoved(buffer,
					startLine,start,numLines,length);

				if(s.start == s.end)
				{
					selection.removeElement(s);
					delayedRepaint(oldStartLine,oldEndLine);
					i--;
				}
				else if(changed)
				{
					delayedRepaint(s.startLine,s.endLine);
				}
			} 

			int scrollMode = (focusedComponent == JEditTextArea.this
				? NORMAL_SCROLL : NO_SCROLL);

			if(caret > start)
			{
				if(caret <= start + length)
					moveCaretPosition(start,scrollMode);
				else
					moveCaretPosition(caret - length,scrollMode);
			}
			else
			{
				
				moveCaretPosition(caret,scrollMode);
			}

			
			if(!buffer.isTransactionInProgress())
				transactionComplete(buffer);
		}
		

		
		public void transactionComplete(Buffer buffer)
		{
			if(delayedUpdate)
			{
				chunkCache.invalidateChunksFromPhys(delayedRepaintStart);

				for(int i = delayedRepaintStart;
					i <= delayedRepaintEnd;
					i++)
				{
					if(displayManager.isLineVisible(i))
						displayManager.getScreenLineCount(i);
				}
				displayManager.notifyScreenLineChanges();

				if(delayedMultilineUpdate)
				{
					invalidateScreenLineRange(chunkCache
						.getScreenLineOfOffset(
						delayedRepaintStart,0),
						visibleLines);
					delayedMultilineUpdate = false;
				}
				else
				{
					invalidateLineRange(delayedRepaintStart,
						delayedRepaintEnd);
				}

				delayedUpdate = false;
			}

			for(int i = 0; i < runnables.size(); i++)
				((Runnable)runnables.get(i)).run();
			runnables.clear();
		} 

		
		private void delayedRepaint(int startLine, int endLine)
		{
			if(!delayedUpdate)
			{
				delayedRepaintStart = startLine;
				delayedRepaintEnd = endLine;
				delayedUpdate = true;
			}
			else
			{
				delayedRepaintStart = Math.min(
					delayedRepaintStart,
					startLine);
				delayedRepaintEnd = Math.max(
					delayedRepaintEnd,
					endLine);
			}
		} 
	} 

	
	class FocusHandler implements FocusListener
	{
		
		public void focusGained(FocusEvent evt)
		{
			if(bufferChanging)
				return;

			if(bracketLine != -1)
				invalidateLineRange(bracketLine,caretLine);
			else
				invalidateLine(caretLine);
		} 

		
		public void focusLost(FocusEvent evt)
		{
			if(!isShowing())
				return;

			if(bracketLine != -1)
				invalidateLineRange(bracketLine,caretLine);
			else
				invalidateLine(caretLine);
		} 
	} 

	
	class MouseHandler extends MouseInputAdapter
	{
		private int dragStartLine;
		private int dragStartOffset;
		private int dragStart;
		private int clickCount;
		private boolean dragged;
		private boolean quickCopyDrag;
		private boolean clearStatus;
		private boolean control;

		
		public void mousePressed(MouseEvent evt)
		{
			control = (OperatingSystem.isMacOS() && evt.isMetaDown())
				|| (!OperatingSystem.isMacOS() && evt.isControlDown());

			
			
			view.getInputHandler().resetLastActionCount();

			grabFocus();
			focusedComponent = JEditTextArea.this;

			if(!buffer.isLoaded())
				return;

			quickCopyDrag = (isQuickCopyEnabled()
				&& GUIUtilities.isMiddleButton(evt.getModifiers()));
			int x = evt.getX();
			int y = evt.getY();

			dragStart = xyToOffset(x,y,!(painter.isBlockCaretEnabled()
				|| isOverwriteEnabled()));
			dragStartLine = getLineOfOffset(dragStart);
			dragStartOffset = dragStart - getLineStartOffset(dragStartLine);

			if(GUIUtilities.isPopupTrigger(evt) && popup != null)
			{
				if(popup.isVisible())
					popup.setVisible(false);
				else
				{
					if(getSelectionCount() == 0 || multi)
						moveCaretPosition(dragStart,false);
					GUIUtilities.showPopupMenu(popup,painter,
						evt.getX(),evt.getY());
				}
				return;
			}

			dragged = false;

			blink = true;
			invalidateLine(caretLine);

			clickCount = evt.getClickCount();

			switch(clickCount)
			{
			case 1:
				doSingleClick(evt);
				break;
			case 2:
				doDoubleClick(evt);
				break;
			default: 
				doTripleClick(evt);
				break;
			}
		} 

		
		private void doSingleClick(MouseEvent evt)
		{
			
			int x = evt.getX();

			float dragStartLineWidth = offsetToXY(dragStartLine,
				getLineLength(dragStartLine),returnValue).x;
			int extraEndVirt;
			if(x > dragStartLineWidth)
			{
				extraEndVirt = (int)((x - dragStartLineWidth)
					/ charWidth);
				if(!getPainter().isBlockCaretEnabled()
					&& !isOverwriteEnabled()
					&& (x - getHorizontalOffset()) % charWidth > charWidth / 2)
						extraEndVirt++;
			}
			else
				extraEndVirt = 0;

			if(control || isRectangularSelectionEnabled())
			{
				int screenLine = (evt.getY() / getPainter()
					.getFontMetrics().getHeight());
				if(screenLine > screenLastLine)
					screenLine = screenLastLine;
				ChunkCache.LineInfo info = chunkCache.getLineInfo(screenLine);
				if(info.lastSubregion)
				{
					
					
					String whitespace = MiscUtilities
						.createWhiteSpace(extraEndVirt,0);
					buffer.insert(dragStart,whitespace);

					dragStart += whitespace.length();
				}
			}

			if(evt.isShiftDown())
			{
				
				resizeSelection(getMarkPosition(),dragStart,extraEndVirt,
					isRectangularSelectionEnabled()
					|| control);

				if(!quickCopyDrag)
					moveCaretPosition(dragStart,false);

				
				dragStartLine = getMarkLine();
				dragStart = getMarkPosition();
				dragStartOffset = dragStart
					- getLineStartOffset(dragStartLine);

				
				dragged = true;

				return;
			}

			if(!quickCopyDrag)
				moveCaretPosition(dragStart,false);

			if(!(multi || quickCopyDrag))
				selectNone();
		} 

		
		private void doDoubleClick(MouseEvent evt)
		{
			
			if(getLineLength(dragStartLine) == 0)
				return;

			String lineText = getLineText(dragStartLine);
			String noWordSep = buffer.getStringProperty("noWordSep");
			if(dragStartOffset == getLineLength(dragStartLine))
				dragStartOffset--;

			boolean joinNonWordChars =
				jEdit.getBooleanProperty("view.joinNonWordChars");
			int wordStart = TextUtilities.findWordStart(lineText,
				dragStartOffset,noWordSep,joinNonWordChars);
			int wordEnd = TextUtilities.findWordEnd(lineText,
				dragStartOffset+1,noWordSep,joinNonWordChars);

			int lineStart = getLineStartOffset(dragStartLine);
			addToSelection(new Selection.Range(lineStart + wordStart,
				lineStart + wordEnd));

			if(quickCopyDrag)
				quickCopyDrag = false;

			moveCaretPosition(lineStart + wordEnd,false);

			
			
			dragged = true;
		} 

		
		private void doTripleClick(MouseEvent evt)
		{
			int newCaret = getLineEndOffset(dragStartLine);
			if(dragStartLine == buffer.getLineCount() - 1)
				newCaret--;

			addToSelection(new Selection.Range(
				getLineStartOffset(dragStartLine),
				newCaret));

			if(quickCopyDrag)
				quickCopyDrag = false;

			moveCaretPosition(newCaret);
		} 

		
		public void mouseDragged(MouseEvent evt)
		{
			if(GUIUtilities.isPopupTrigger(evt)
				|| (popup != null && popup.isVisible()))
				return;

			if(!buffer.isLoaded())
				return;

			if(evt.getY() < 0)
			{
				int delta = Math.min(-1,evt.getY()
					/ painter.getFontMetrics()
					.getHeight());
				setFirstLine(getFirstLine() + delta);
			}
			else if(evt.getY() >= painter.getHeight())
			{
				int delta = Math.max(1,(evt.getY()
					- painter.getHeight()) /
					painter.getFontMetrics()
					.getHeight());
				if(lastLinePartial)
					delta--;
				setFirstLine(getFirstLine() + delta);
			}

			if(quickCopyDrag)
			{
				view.getStatus().setMessage(jEdit.getProperty(
					"view.status.rect-quick-copy"));
				clearStatus = true;
			}

			switch(clickCount)
			{
			case 1:
				doSingleDrag(evt);
				break;
			case 2:
				doDoubleDrag(evt);
				break;
			default: 
				doTripleDrag(evt);
				break;
			}
		} 

		
		private void doSingleDrag(MouseEvent evt)
		{
			dragged = true;

			int x = evt.getX();
			int y = evt.getY();
			if(y < 0)
				y = 0;
			else if(y >= painter.getHeight())
				y = painter.getHeight() - 1;

			int dot = xyToOffset(x,y,
				(!painter.isBlockCaretEnabled()
				&& !isOverwriteEnabled())
				|| quickCopyDrag);
			int dotLine = buffer.getLineOfOffset(dot);
			int extraEndVirt = 0;

			if(chunkCache.getLineInfo(screenLastLine).lastSubregion)
			{
				float dotLineWidth = offsetToXY(dotLine,getLineLength(dotLine),
					returnValue).x;
				if(x > dotLineWidth)
				{
					extraEndVirt = (int)((x - dotLineWidth) / charWidth);
					if(!getPainter().isBlockCaretEnabled()
						&& !isOverwriteEnabled()
						&& (x - getHorizontalOffset()) % charWidth > charWidth / 2)
						extraEndVirt++;
				}
			}

			resizeSelection(dragStart,dot,extraEndVirt,
				isRectangularSelectionEnabled()
				|| control);

			if(quickCopyDrag)
			{
				
				scrollTo(dotLine,dot - buffer.getLineStartOffset(dotLine),false);
			}
			else
			{
				if(dot != caret)
					moveCaretPosition(dot,false);
				if(isRectangularSelectionEnabled()
					&& extraEndVirt != 0)
				{
					scrollTo(dotLine,dot - buffer.getLineStartOffset(dotLine)
						+ extraEndVirt,false);
				}
			}
		} 

		
		private void doDoubleDrag(MouseEvent evt)
		{
			int markLineStart = getLineStartOffset(dragStartLine);
			int markLineLength = getLineLength(dragStartLine);
			int mark = dragStartOffset;

			int pos = xyToOffset(evt.getX(),
				Math.max(0,Math.min(painter.getHeight(),evt.getY())),
				!(painter.isBlockCaretEnabled() || isOverwriteEnabled()));
			int line = getLineOfOffset(pos);
			int lineStart = getLineStartOffset(line);
			int lineLength = getLineLength(line);
			int offset = pos - lineStart;

			String lineText = getLineText(line);
			String markLineText = getLineText(dragStartLine);
			String noWordSep = buffer.getStringProperty("noWordSep");
			boolean joinNonWordChars =
				jEdit.getBooleanProperty("view.joinNonWordChars");

			if(markLineStart + dragStartOffset > lineStart + offset)
			{
				if(offset != 0 && offset != lineLength)
				{
					offset = TextUtilities.findWordStart(
						lineText,offset,noWordSep,
						joinNonWordChars);
				}

				if(markLineLength != 0)
				{
					mark = TextUtilities.findWordEnd(
						markLineText,mark,noWordSep,
						joinNonWordChars);
				}
			}
			else
			{
				if(offset != 0 && lineLength != 0)
				{
					offset = TextUtilities.findWordEnd(
						lineText,offset,noWordSep,
						joinNonWordChars);
				}

				if(mark != 0 && mark != markLineLength)
				{
					mark = TextUtilities.findWordStart(
						markLineText,mark,noWordSep,
						joinNonWordChars);
				}
			}

			if(lineStart + offset == caret)
				return;

			resizeSelection(markLineStart + mark,lineStart + offset,
				0,false);
			if(!quickCopyDrag)
				moveCaretPosition(lineStart + offset,false);
		} 

		
		private void doTripleDrag(MouseEvent evt)
		{
			int offset = xyToOffset(evt.getX(),
				Math.max(0,Math.min(painter.getHeight(),evt.getY())),
				false);
			int mouseLine = getLineOfOffset(offset);
			int mark;
			int mouse;
			if(dragStartLine > mouseLine)
			{
				mark = getLineEndOffset(dragStartLine) - 1;
				if(offset == getLineEndOffset(mouseLine) - 1)
					mouse = offset;
				else
					mouse = getLineStartOffset(mouseLine);
			}
			else
			{
				mark = getLineStartOffset(dragStartLine);
				if(offset == getLineStartOffset(mouseLine))
					mouse = offset;
				else if(offset == getLineEndOffset(mouseLine) - 1
					&& mouseLine != getBuffer().getLineCount() - 1)
					mouse = getLineEndOffset(mouseLine);
				else
					mouse = getLineEndOffset(mouseLine) - 1;
			}

			mouse = Math.min(getBuffer().getLength(),mouse);

			if(mouse == caret)
				return;

			dragged = true;

			resizeSelection(mark,mouse,0,false);
			moveCaretPosition(mouse,false);
		} 

		
		public void mouseReleased(MouseEvent evt)
		{
			Selection sel = null;

			if(control && !dragged)
			{
				int offset = xyToOffset(evt.getX(),evt.getY(),false);
				if(offset != buffer.getLength())
				{
					buffer.getText(offset,1,lineSegment);
					switch(lineSegment.array[lineSegment.offset])
					{
					case '(': case '[': case '{':
					case ')': case ']': case '}':
						if(evt.isShiftDown())
						{
							moveCaretPosition(offset,false);
							narrowToMatchingBracket(offset);
						}
						else
						{
							if(!quickCopyDrag)
								moveCaretPosition(offset,false);
							sel = selectToMatchingBracket(offset,true);
							dragged = true;
						}
						break;
					}
				}
			}

			
			
			if(sel == null)
				sel = getSelectionAtOffset(dragStart);
			if(dragged && sel != null)
			{
				Registers.setRegister('%',getSelectedText(sel));
				if(quickCopyDrag)
				{
					removeFromSelection(sel);
					Registers.paste(JEditTextArea.this,'%',
						sel instanceof Selection.Rect);
				}
			}
			else if(isQuickCopyEnabled()
				&& GUIUtilities.isMiddleButton(evt.getModifiers()))
			{
				setCaretPosition(dragStart,false);
				if(!isEditable())
					getToolkit().beep();
				else
					Registers.paste(JEditTextArea.this,'%',control);
			}

			dragged = false;

			if(clearStatus)
			{
				clearStatus = false;
				view.getStatus().setMessage(null);
			}
		} 
	} 

	

	
	static
	{
		caretTimer = new Timer(500,new CaretBlinker());
		caretTimer.setInitialDelay(500);
		caretTimer.start();

		bracketTimer = new Timer(200,new ActionListener()
		{
			public void actionPerformed(ActionEvent evt)
			{
				if(focusedComponent != null)
					focusedComponent.updateBracketHighlight();
			}
		});
		bracketTimer.setInitialDelay(200);
		bracketTimer.setRepeats(false);
	} 
}
