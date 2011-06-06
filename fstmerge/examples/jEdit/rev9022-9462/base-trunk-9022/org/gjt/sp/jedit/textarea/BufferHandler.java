

package org.gjt.sp.jedit.textarea;

import java.util.Iterator;
import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.Debug;


class BufferHandler implements BufferListener
{
	private DisplayManager displayManager;
	private TextArea textArea;
	private JEditBuffer buffer;

	boolean delayedUpdate;
	boolean delayedMultilineUpdate;
	int delayedUpdateStart;
	int delayedUpdateEnd;

	
	BufferHandler(DisplayManager displayManager,
		TextArea textArea,
		JEditBuffer buffer)
	{
		this.displayManager = displayManager;
		this.textArea = textArea;
		this.buffer = buffer;
	} 

	
	public void bufferLoaded(JEditBuffer buffer)
	{
		displayManager.bufferLoaded();
	} 

	
	public void foldHandlerChanged(JEditBuffer buffer)
	{
		displayManager.foldHandlerChanged();
	} 

	
	public void foldLevelChanged(JEditBuffer buffer, int start, int end)
	{
		

		if(textArea.getDisplayManager() == displayManager
			&& end != 0 && !buffer.isLoading())
		{
			textArea.invalidateLineRange(start - 1,
				textArea.getLastPhysicalLine());
		}
	} 

	
	public void contentInserted(JEditBuffer buffer, int startLine,
		int offset, int numLines, int length)
	{
		if(buffer.isLoading())
			return;

		displayManager.screenLineMgr.contentInserted(startLine,numLines);

		int endLine = startLine + numLines;

		if(numLines != 0)
			delayedMultilineUpdate = true;

		displayManager.folds.contentInserted(startLine,numLines);

		FirstLine firstLine = displayManager.firstLine;
		ScrollLineCount scrollLineCount = displayManager.scrollLineCount;

		if(textArea.getDisplayManager() == displayManager)
		{
			if(numLines != 0)
			{
				firstLine.contentInserted(startLine,numLines);
				scrollLineCount.contentInserted(startLine,numLines);
			}

			if(delayedUpdateEnd >= startLine)
				delayedUpdateEnd += numLines;
			delayUpdate(startLine,endLine);

			
			Iterator<Selection> iter = textArea.getSelectionIterator();
			while(iter.hasNext())
			{
				Selection s = iter.next();

				if(s.contentInserted(buffer,startLine,offset,
					numLines,length))
				{
					delayUpdate(s.startLine,s.endLine);
				}
			} 

			int caret = textArea.getCaretPosition();
			if(caret >= offset)
			{
				int scrollMode = (textArea.caretAutoScroll()
					? TextArea.ELECTRIC_SCROLL
					: TextArea.NO_SCROLL);
				textArea.moveCaretPosition(
					caret + length,scrollMode);
			}
			else
			{
				int scrollMode = (textArea.caretAutoScroll()
					? TextArea.NORMAL_SCROLL
					: TextArea.NO_SCROLL);
				textArea.moveCaretPosition(
					caret,scrollMode);
			}
		}
		else
		{
			firstLine.callReset = true;
			scrollLineCount.callReset = true;
		}
	} 

	
	
	public void preContentRemoved(JEditBuffer buffer, int startLine,
		int offset, int numLines, int length)
	{
		if(buffer.isLoading())
			return;

		FirstLine firstLine = displayManager.firstLine;
		ScrollLineCount scrollLineCount = displayManager.scrollLineCount;

		if(textArea.getDisplayManager() == displayManager)
		{
			if(numLines != 0)
			{
				firstLine.preContentRemoved(startLine,numLines);
				scrollLineCount.preContentRemoved(startLine,numLines);
			}

			if(delayedUpdateEnd >= startLine)
				delayedUpdateEnd -= numLines;
			delayUpdate(startLine,startLine);
		}
		else
		{
			firstLine.callReset = true;
			scrollLineCount.callReset = true;
		}

		displayManager.screenLineMgr.contentRemoved(startLine,numLines);

		if(numLines == 0)
			return;

		delayedMultilineUpdate = true;

		if(displayManager.folds.preContentRemoved(startLine,numLines))
		{
			displayManager.folds.reset(buffer.getLineCount());
			firstLine.callReset = true;
			scrollLineCount.callReset = true;
		}

		if(firstLine.physicalLine
			> displayManager.getLastVisibleLine()
			|| firstLine.physicalLine
			< displayManager.getFirstVisibleLine())
		{
			
			
			
		}
		
		
		
		
		else if(!displayManager.isLineVisible(
			firstLine.physicalLine))
		{
			firstLine.physicalLine =
				displayManager.getNextVisibleLine(
				firstLine.physicalLine);
		}
	} 

	
	public void contentRemoved(JEditBuffer buffer, int startLine,
		int start, int numLines, int length)
	{
		if(buffer.isLoading())
			return;

		if(textArea.getDisplayManager() == displayManager)
		{
			
			Iterator<Selection> iter = textArea.getSelectionIterator();
			while(iter.hasNext())
			{
				Selection s = iter.next();

				if(s.contentRemoved(buffer,startLine,
					start,numLines,length))
				{
					delayUpdate(s.startLine,s.endLine);
					if(s.start == s.end)
						iter.remove();
				}
			} 

			int caret = textArea.getCaretPosition();

			if(caret >= start + length)
			{
				int scrollMode = (textArea.caretAutoScroll()
					? TextArea.ELECTRIC_SCROLL
					: TextArea.NO_SCROLL);
				textArea.moveCaretPosition(
					caret - length,
					scrollMode);
			}
			else if(caret >= start)
			{
				int scrollMode = (textArea.caretAutoScroll()
					? TextArea.ELECTRIC_SCROLL
					: TextArea.NO_SCROLL);
				textArea.moveCaretPosition(
					start,scrollMode);
			}
			else
			{
				int scrollMode = (textArea.caretAutoScroll()
					? TextArea.NORMAL_SCROLL
					: TextArea.NO_SCROLL);
				textArea.moveCaretPosition(caret,scrollMode);
			}
		}
	}
	

	
	public void transactionComplete(JEditBuffer buffer)
	{
		if(textArea.getDisplayManager() != displayManager)
		{
			delayedUpdate = false;
			return;
		}

		if(delayedUpdate)
			doDelayedUpdate();

		textArea._finishCaretUpdate();

		delayedUpdate = false;

		
		if(Debug.SCROLL_VERIFY)
		{
			int line = delayedUpdateStart;
			if(!displayManager.isLineVisible(line))
				line = displayManager.getNextVisibleLine(line);
			System.err.println(delayedUpdateStart + ":" + delayedUpdateEnd + ':' + textArea.getLineCount());
			int scrollLineCount = 0;
			while(line != -1 && line <= delayedUpdateEnd)
			{
				scrollLineCount += displayManager.getScreenLineCount(line);
				line = displayManager.getNextVisibleLine(line);
			}

			if(scrollLineCount != displayManager.getScrollLineCount())
			{
				throw new InternalError(scrollLineCount
					+ " != "
					+ displayManager.getScrollLineCount());
			}
		} 
	} 

	
	private void doDelayedUpdate()
	{
		
		
		
		int line = delayedUpdateStart;
		if(!displayManager.isLineVisible(line))
			line = displayManager.getNextVisibleLine(line);
		while(line != -1 && line <= delayedUpdateEnd)
		{
			displayManager.updateScreenLineCount(line);
			line = displayManager.getNextVisibleLine(line);
		}

		
		
		
		
		displayManager.notifyScreenLineChanges();

		if(delayedMultilineUpdate)
		{
			textArea.invalidateScreenLineRange(
				textArea.chunkCache
				.getScreenLineOfOffset(
				delayedUpdateStart,0),
				textArea.getVisibleLines());
			delayedMultilineUpdate = false;
		}
		else
		{
			textArea.invalidateLineRange(
				delayedUpdateStart,
				delayedUpdateEnd);
		}

		
		int visibleLines = textArea.getVisibleLines();
		if(visibleLines != 0)
		{
			textArea.chunkCache.getLineInfo(
				visibleLines - 1);
		}

		
		

		
		
		
		
		
		

		buffer.getFoldLevel(delayedUpdateEnd);
	} 

	
	private void delayUpdate(int startLine, int endLine)
	{
		textArea.chunkCache.invalidateChunksFromPhys(startLine);
		textArea.repaintMgr.setFastScroll(false);

		if(!delayedUpdate)
		{
			delayedUpdateStart = startLine;
			delayedUpdateEnd = endLine;
			delayedUpdate = true;
		}
		else
		{
			delayedUpdateStart = Math.min(
				delayedUpdateStart,
				startLine);
			delayedUpdateEnd = Math.max(
				delayedUpdateEnd,
				endLine);
		}
	} 
}
