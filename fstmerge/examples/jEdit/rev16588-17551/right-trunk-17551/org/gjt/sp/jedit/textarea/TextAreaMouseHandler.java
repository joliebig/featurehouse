
package org.gjt.sp.jedit.textarea;

import org.gjt.sp.jedit.OperatingSystem;
import org.gjt.sp.jedit.TextUtilities;
import org.gjt.sp.util.StandardUtilities;

import javax.swing.event.MouseInputAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.InputEvent;
import java.awt.*;


public class TextAreaMouseHandler extends MouseInputAdapter
{
	
	TextAreaMouseHandler(TextArea textArea)
	{
		this.textArea = textArea;
	} 

	
	@Override
	public void mousePressed(MouseEvent evt)
	{
		showCursor();

		control = (OperatingSystem.isMacOS() && evt.isMetaDown())
			|| (!OperatingSystem.isMacOS() && evt.isControlDown());

		ctrlForRectangularSelection = true;

		
		
		textArea.getInputHandler().resetLastActionCount();

		quickCopyDrag = (textArea.isQuickCopyEnabled() &&
			isMiddleButton(evt.getModifiers()));

		if(!quickCopyDrag)
		{
			textArea.requestFocus();
			TextArea.focusedComponent = textArea;
		}

		if(textArea.getBuffer().isLoading())
			return;

		int x = evt.getX();
		int y = evt.getY();

		dragStart = textArea.xyToOffset(x,y,
			!(textArea.getPainter().isBlockCaretEnabled()
			|| textArea.isOverwriteEnabled()));
		dragStartLine = textArea.getLineOfOffset(dragStart);
		dragStartOffset = dragStart - textArea.getLineStartOffset(
			dragStartLine);

		if(isPopupTrigger(evt) && textArea.isRightClickPopupEnabled())
		{
				textArea.handlePopupTrigger(evt);
				return;
		}

		dragged = false;

		textArea.blink = true;
		textArea.invalidateLine(textArea.getCaretLine());

		clickCount = evt.getClickCount();

		if(textArea.isDragEnabled()
			&& textArea.selectionManager.insideSelection(x,y)
			&& clickCount == 1 && !evt.isShiftDown())
		{
			maybeDragAndDrop = true;
			textArea.moveCaretPosition(dragStart,false);
			return;
		}
		maybeDragAndDrop = false;

		if(quickCopyDrag)
		{
			
			doSingleClick(evt);
		}
		else
		{
			switch(clickCount)
			{
			case 1:
				doSingleClick(evt);
				break;
			case 2:
				doDoubleClick();
				break;
			default: 
				doTripleClick();
				break;
			}
		}
	} 

	
	protected void doSingleClick(MouseEvent evt)
	{
		int x = evt.getX();

		int extraEndVirt = 0;
		if(textArea.chunkCache.getLineInfo(
			textArea.getLastScreenLine()).lastSubregion)
		{
			int dragStart = textArea.xyToOffset(x,evt.getY(),
				!textArea.getPainter().isBlockCaretEnabled()
				&& !textArea.isOverwriteEnabled());
			int screenLine = textArea.getScreenLineOfOffset(dragStart);
			ChunkCache.LineInfo lineInfo = textArea.chunkCache.getLineInfo(screenLine);
			int offset = textArea.getScreenLineEndOffset(screenLine);
			if ((1 != offset - dragStart) || (lineInfo.lastSubregion))
			{
				offset--;
			}
			float dragStartLineWidth = textArea.offsetToXY(offset).x;
			if(x > dragStartLineWidth)
			{
				extraEndVirt = (int)(
					(x - dragStartLineWidth)
					/ textArea.charWidth);
				if(!textArea.getPainter().isBlockCaretEnabled()
					&& !textArea.isOverwriteEnabled()
					&& (x - textArea.getHorizontalOffset())
					% textArea.charWidth > textArea.charWidth / 2)
				{
					extraEndVirt++;
				}
			}
		}

		if(((control && ctrlForRectangularSelection) ||
		    textArea.isRectangularSelectionEnabled())
			&& textArea.isEditable())
		{
			int screenLine = (evt.getY() / textArea.getPainter()
				.getFontMetrics().getHeight());
			if(screenLine > textArea.getLastScreenLine())
				screenLine = textArea.getLastScreenLine();
			ChunkCache.LineInfo info = textArea.chunkCache.getLineInfo(screenLine);
			if(info.lastSubregion && extraEndVirt != 0)
			{
				
				
				String whitespace = StandardUtilities
					.createWhiteSpace(extraEndVirt,0);
				textArea.getBuffer().insert(dragStart,whitespace);

				dragStart += whitespace.length();
			}
		}

		if(evt.isShiftDown())
		{
			
			textArea.resizeSelection(
				textArea.getMarkPosition(),dragStart,extraEndVirt,
				textArea.isRectangularSelectionEnabled()
				|| (control && ctrlForRectangularSelection));

			if(!quickCopyDrag)
				textArea.moveCaretPosition(dragStart,false);

			
			dragStartLine = textArea.getMarkLine();
			dragStart = textArea.getMarkPosition();
			dragStartOffset = dragStart
				- textArea.getLineStartOffset(dragStartLine);

			
			dragged = true;

			return;
		}

		if(!quickCopyDrag)
			textArea.moveCaretPosition(dragStart,false);

		if(!(textArea.isMultipleSelectionEnabled()
			|| quickCopyDrag))
			textArea.selectNone();
	} 

	
	protected void doDoubleClick()
	{
		
		if(textArea.getLineLength(dragStartLine) == 0)
			return;

		String lineText = textArea.getLineText(dragStartLine);
		String noWordSep = textArea.getBuffer()
			.getStringProperty("noWordSep");
		if(dragStartOffset == textArea.getLineLength(dragStartLine))
			dragStartOffset--;

		boolean joinNonWordChars = textArea.getJoinNonWordChars();
		int wordStart = TextUtilities.findWordStart(lineText,dragStartOffset,
			noWordSep,joinNonWordChars,false,false);
		int wordEnd = TextUtilities.findWordEnd(lineText,
			dragStartOffset+1,noWordSep,
			joinNonWordChars,false,false);

		int lineStart = textArea.getLineStartOffset(dragStartLine);
		Selection sel = new Selection.Range(
			lineStart + wordStart,
			lineStart + wordEnd);
		if(textArea.isMultipleSelectionEnabled())
			textArea.addToSelection(sel);
		else
			textArea.setSelection(sel);

		if(quickCopyDrag)
			quickCopyDrag = false;

		textArea.moveCaretPosition(lineStart + wordEnd,false);

		dragged = true;
	} 

	
	protected void doTripleClick()
	{
		int newCaret = textArea.getLineEndOffset(dragStartLine);
		if(dragStartLine == textArea.getLineCount() - 1)
			newCaret--;

		Selection sel = new Selection.Range(
			textArea.getLineStartOffset(dragStartLine),
			newCaret);
		if(textArea.isMultipleSelectionEnabled())
			textArea.addToSelection(sel);
		else
			textArea.setSelection(sel);

		if(quickCopyDrag)
			quickCopyDrag = false;

		textArea.moveCaretPosition(newCaret,false);

		dragged = true;
	} 

	
	@Override
	public void mouseMoved(MouseEvent evt)
	{
		showCursor();
	} 

	
	@Override
	public void mouseDragged(MouseEvent evt)
	{
		if (isPopupTrigger(evt))
			return;

		if(maybeDragAndDrop)
		{
			textArea.startDragAndDrop(evt,control);
			return;
		}

		if(textArea.getBuffer().isLoading())
			return;

		TextAreaPainter painter = textArea.getPainter();
		if(evt.getY() < 0)
		{
			int delta = Math.min(-1,evt.getY()
				/ painter.getFontMetrics()
				.getHeight());
			textArea.setFirstLine(textArea.getFirstLine() + delta);
		}
		else if(evt.getY() >= painter.getHeight())
		{
			int delta = Math.max(1,(evt.getY()
				- painter.getHeight()) /
				painter.getFontMetrics()
				.getHeight());
			if(textArea.lastLinePartial)
				delta--;
			textArea.setFirstLine(textArea.getFirstLine() + delta);
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

		TextAreaPainter painter = textArea.getPainter();

		int x = evt.getX();
		int y = evt.getY();
		if(y < 0)
			y = 0;
		else if(y >= painter.getHeight())
			y = painter.getHeight() - 1;

		int dot = textArea.xyToOffset(x,y,
			(!painter.isBlockCaretEnabled()
			&& !textArea.isOverwriteEnabled())
			|| quickCopyDrag);
		int dotLine = textArea.getLineOfOffset(dot);
		int extraEndVirt = 0;

		if(textArea.chunkCache.getLineInfo(
			textArea.getLastScreenLine())
			.lastSubregion)
		{
			int screenLine = textArea.getScreenLineOfOffset(dot);
			ChunkCache.LineInfo lineInfo = textArea.chunkCache.getLineInfo(screenLine);
			int offset = textArea.getScreenLineEndOffset(screenLine);
			if ((1 != offset - dot) || (lineInfo.lastSubregion))
			{
				offset--;
			}
			float dotLineWidth = textArea.offsetToXY(offset).x;
			if(x > dotLineWidth)
			{
				extraEndVirt = (int)((x - dotLineWidth) / textArea.charWidth);
				if(!painter.isBlockCaretEnabled()
					&& !textArea.isOverwriteEnabled()
					&& (x - textArea.getHorizontalOffset()) % textArea.charWidth > textArea.charWidth / 2)
					extraEndVirt++;
			}
		}

		textArea.resizeSelection(dragStart,dot,extraEndVirt,
			textArea.isRectangularSelectionEnabled()
			|| (control && ctrlForRectangularSelection));

		if(quickCopyDrag)
		{
			
			textArea.scrollTo(dotLine,dot - textArea.getLineStartOffset(dotLine),false);
		}
		else
		{
			if(dot != textArea.getCaretPosition())
				textArea.moveCaretPosition(dot,false);
			if(textArea.isRectangularSelectionEnabled()
				&& extraEndVirt != 0)
			{
				textArea.scrollTo(dotLine,dot - textArea.getLineStartOffset(dotLine)
					+ extraEndVirt,false);
			}
		}
	} 

	
	private void doDoubleDrag(MouseEvent evt)
	{
		int markLineStart = textArea.getLineStartOffset(dragStartLine);
		int markLineLength = textArea.getLineLength(dragStartLine);
		int mark = dragStartOffset;

		TextAreaPainter painter = textArea.getPainter();

		int pos = textArea.xyToOffset(evt.getX(),
			Math.max(0,Math.min(painter.getHeight(),evt.getY())),
			!(painter.isBlockCaretEnabled()
			|| textArea.isOverwriteEnabled()));
		int line = textArea.getLineOfOffset(pos);
		int lineStart = textArea.getLineStartOffset(line);
		int lineLength = textArea.getLineLength(line);
		int offset = pos - lineStart;

		String lineText = textArea.getLineText(line);
		String markLineText = textArea.getLineText(dragStartLine);
		String noWordSep = textArea.getBuffer()
			.getStringProperty("noWordSep");
		boolean joinNonWordChars = textArea.getJoinNonWordChars();

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

		if(lineStart + offset == textArea.getCaretPosition())
			return;

		textArea.resizeSelection(markLineStart + mark,
			lineStart + offset,0,false);
		textArea.moveCaretPosition(lineStart + offset,false);

		dragged = true;
	} 

	
	private void doTripleDrag(MouseEvent evt)
	{
		TextAreaPainter painter = textArea.getPainter();

		int offset = textArea.xyToOffset(evt.getX(),
			Math.max(0,Math.min(painter.getHeight(),evt.getY())),
			false);
		int mouseLine = textArea.getLineOfOffset(offset);
		int mark;
		int mouse;
		if(dragStartLine > mouseLine)
		{
			mark = textArea.getLineEndOffset(dragStartLine) - 1;
			if(offset == textArea.getLineEndOffset(mouseLine) - 1)
				mouse = offset;
			else
				mouse = textArea.getLineStartOffset(mouseLine);
		}
		else
		{
			mark = textArea.getLineStartOffset(dragStartLine);
			if(offset == textArea.getLineStartOffset(mouseLine))
				mouse = offset;
			else if(offset == textArea.getLineEndOffset(mouseLine) - 1
				&& mouseLine != textArea.getLineCount() - 1)
				mouse = textArea.getLineEndOffset(mouseLine);
			else
				mouse = textArea.getLineEndOffset(mouseLine) - 1;
		}

		mouse = Math.min(textArea.getBuffer().getLength(),mouse);

		if(mouse == textArea.getCaretPosition())
			return;

		textArea.resizeSelection(mark,mouse,0,false);
		textArea.moveCaretPosition(mouse,false);

		dragged = true;
	} 

	
	@Override
	public void mouseReleased(MouseEvent evt)
	{
		if(!dragged && textArea.isQuickCopyEnabled() &&
			isMiddleButton(evt.getModifiers()))
		{
			textArea.requestFocus();
			TextArea.focusedComponent = textArea;

			textArea.setCaretPosition(dragStart,false);
		}
		else if(maybeDragAndDrop
			&& !textArea.isMultipleSelectionEnabled())
		{
			textArea.selectNone();
		}

		dragged = false;
	} 

	
	
	public static boolean isPopupTrigger(MouseEvent evt)
	{
		return isRightButton(evt.getModifiers());
	} 

	
	
	public static boolean isMiddleButton(int modifiers)
	{
		if (OperatingSystem.isMacOS())
		{
			if((modifiers & InputEvent.BUTTON1_MASK) != 0)
				return (modifiers & InputEvent.ALT_MASK) != 0;
			else
				return (modifiers & InputEvent.BUTTON2_MASK) != 0;
		}
		else
			return (modifiers & InputEvent.BUTTON2_MASK) != 0;
	} 

	
	
	public static boolean isRightButton(int modifiers)
	{
		if (OperatingSystem.isMacOS())
		{
			if((modifiers & InputEvent.BUTTON1_MASK) != 0)
				return (modifiers & InputEvent.CTRL_MASK) != 0;
			else
				return (modifiers & InputEvent.BUTTON3_MASK) != 0;
		}
		else
			return (modifiers & InputEvent.BUTTON3_MASK) != 0;
	} 

	
	protected final TextArea textArea;
	protected int dragStartLine;
	protected int dragStartOffset;
	protected int dragStart;
	protected int clickCount;
	protected boolean dragged;
	protected boolean quickCopyDrag;
	protected boolean control;
	protected boolean ctrlForRectangularSelection;
	
	protected boolean maybeDragAndDrop;

	
	protected void showCursor()
	{
		textArea.getPainter().setCursor(
			Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR));
	} 

	
}
