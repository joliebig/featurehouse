

package org.gjt.sp.jedit.buffer;


import javax.swing.text.*;
import org.gjt.sp.jedit.syntax.*;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.Debug;
import org.gjt.sp.util.IntegerArray;
import org.gjt.sp.util.Log;



public class OffsetManager
{
	public static final long MAX_DISPLAY_COUNT = 8;

	
	public static final int FOLD_LEVEL_MASK         = 0x000000ff;
	public static final int VISIBLE_MASK            = 0x0000ff00;
	public static final int VISIBLE_SHIFT           = 8;
	public static final int SCREEN_LINES_MASK       = 0x00ff0000;
	public static final int SCREEN_LINES_SHIFT      = 16;
	public static final int SCREEN_LINES_VALID_MASK = (1<<25);

	
	public OffsetManager(Buffer buffer)
	{
		this.buffer = buffer;

		positions = new PosBottomHalf[100];
	} 

	
	public final int getLineCount()
	{
		return lineCount;
	} 

	
	public int getLineOfOffset(int offset)
	{
		int start = 0;
		int end = lineCount - 1;

		for(;;)
		{
			switch(end - start)
			{
			case 0:
				if(getLineEndOffset(start) <= offset)
					return start + 1;
				else
					return start;
			case 1:
				if(getLineEndOffset(start) <= offset)
				{
					if(getLineEndOffset(end) <= offset)
						return end + 1;
					else
						return end;
				}
				else
					return start;
			default:
				int pivot = (end + start) / 2;
				int value = getLineEndOffset(pivot);
				if(value == offset)
					return pivot + 1;
				else if(value < offset)
					start = pivot + 1;
				else
					end = pivot - 1;
				break;
			}
		}
	} 

	
	public final int getLineEndOffset(int line)
	{
		if(gapLine != -1 && line >= gapLine)
			return endOffsets[line] + gapWidth;
		else
			return endOffsets[line];
	} 

	
	public final int getFoldLevel(int line)
	{
		return (lineInfo[line] & FOLD_LEVEL_MASK);
	} 

	
	
	public final void setFoldLevel(int line, int level)
	{
		if(level > 0xff)
		{
			
			level = 0xff;
		}

		lineInfo[line] = ((lineInfo[line] & ~FOLD_LEVEL_MASK) | level);
	} 

	
	public void setFirstInvalidFoldLevel(int firstInvalidFoldLevel)
	{
		this.firstInvalidFoldLevel = firstInvalidFoldLevel;
	} 

	
	public int getFirstInvalidFoldLevel()
	{
		return firstInvalidFoldLevel;
	} 

	
	public final boolean isLineVisible(int line, int index)
	{
		int mask = 1 << (index + VISIBLE_SHIFT);
		return (lineInfo[line] & mask) != 0;
	} 

	
	public final void setLineVisible(int line, int index, boolean visible)
	{
		int info = lineInfo[line];
		int mask = 1 << (index + VISIBLE_SHIFT);
		boolean oldVisible = ((info & mask) != 0);
		if(visible)
		{
			if(!oldVisible)
			{
				int screenLines = getScreenLineCount(line);
				Anchor anchor = anchors;
				for(;;)
				{
					if(anchor == null)
						break;

					if(anchor.physicalLine < line)
						break;

					if(anchor.index == index)
					{
						anchor.scrollLine += screenLines;
						anchor.callChanged = true;
					}
					anchor = anchor.next;
				}
				lineInfo[line] = (info | mask);
			}
		}
		else
		{
			if(oldVisible)
			{
				int screenLines = getScreenLineCount(line);
				Anchor anchor = anchors;
				for(;;)
				{
					if(anchor == null)
						break;

					if(anchor.physicalLine < line)
						break;

					if(anchor.index == index)
					{
						anchor.scrollLine -= screenLines;
						anchor.callChanged = true;
					}
					anchor = anchor.next;
				}
				lineInfo[line] = (info & ~mask);
			}
		}
	} 

	
	public final boolean isScreenLineCountValid(int line)
	{
		return (lineInfo[line] & SCREEN_LINES_VALID_MASK) != 0;
	} 

	
	public final int getScreenLineCount(int line)
	{
		return ((lineInfo[line] & SCREEN_LINES_MASK)
			>> SCREEN_LINES_SHIFT);
	} 

	
	public final void setScreenLineCount(int line, int count)
	{
		if(Debug.SCREEN_LINES_DEBUG)
			Log.log(Log.DEBUG,this,new Exception("setScreenLineCount(" + line + "," + count + ")"));
		int info = lineInfo[line];
		int oldCount = ((info & SCREEN_LINES_MASK)
			>> SCREEN_LINES_SHIFT);
		if(oldCount != count)
		{
			Anchor anchor = anchors;
			for(;;)
			{
				if(anchor == null)
					break;

				if(anchor.physicalLine < line)
					break;

				
				
				
				

				
				
				
				
				if(anchor.physicalLine == line)
					anchor.callChanged = true;
				else
				{
					int anchorVisibilityMask = (1 << (anchor.index + VISIBLE_SHIFT));
					if((info & anchorVisibilityMask) != 0)
					{
						
						
						
						anchor.scrollLine += (count - oldCount);
						anchor.callChanged = true;
					}
				}
				anchor = anchor.next;
			}
		}
		lineInfo[line] = ((info & ~SCREEN_LINES_MASK)
			| (count << SCREEN_LINES_SHIFT)
			| SCREEN_LINES_VALID_MASK);
	} 

	
	public final TokenMarker.LineContext getLineContext(int line)
	{
		return lineContext[line];
	} 

	
	public final void setLineContext(int line, TokenMarker.LineContext context)
	{
		lineContext[line] = context;
	} 

	
	public void setFirstInvalidLineContext(int firstInvalidLineContext)
	{
		this.firstInvalidLineContext = firstInvalidLineContext;
	} 

	
	public int getFirstInvalidLineContext()
	{
		return firstInvalidLineContext;
	} 

	

	
	
	
	

	
	
	
	
	public synchronized Position createPosition(int offset)
	{
		PosBottomHalf bh = null;

		for(int i = 0; i < positionCount; i++)
		{
			PosBottomHalf _bh = positions[i];
			if(_bh.offset == offset)
			{
				bh = _bh;
				break;
			}
			else if(_bh.offset > offset)
			{
				bh = new PosBottomHalf(offset);
				growPositionArray();
				System.arraycopy(positions,i,positions,i+1,
					positionCount - i);
				positionCount++;
				positions[i] = bh;
				break;
			}
		}

		if(bh == null)
		{
			bh = new PosBottomHalf(offset);
			growPositionArray();
			positions[positionCount++] = bh;
		}

		return new PosTopHalf(bh);
	} 

	
	
	public void expandFolds(int foldLevel)
	{
		if(foldLevel == 0)
		{
			for(int i = 0; i < lineCount; i++)
				lineInfo[i] |= VISIBLE_MASK;
		}
		else
		{
			if(buffer.getFoldHandler() instanceof IndentFoldHandler)
				foldLevel = (foldLevel - 1) * buffer.getIndentSize() + 1;

			
			boolean seenVisibleLine = false;

			for(int i = 0; i < lineCount; i++)
			{
				if(!seenVisibleLine || buffer.getFoldLevel(i) < foldLevel)
				{
					seenVisibleLine = true;
					lineInfo[i] |= VISIBLE_MASK;
				}
				else
					lineInfo[i] &= ~VISIBLE_MASK;
			}
		}
	} 

	
	public void resetAnchors()
	{
		if(Debug.SCROLL_DEBUG)
			Log.log(Log.DEBUG,this,"resetAnchors(): " + anchors);
		Anchor anchor = anchors;
		while(anchor != null)
		{
			anchor.reset();
			anchor = anchor.next;
		}
	} 

	
	public void invalidateScreenLineCounts()
	{
		for(int i = 0; i < lineCount; i++)
			lineInfo[i] &= ~SCREEN_LINES_VALID_MASK;
	} 

	
	public void _contentInserted(IntegerArray endOffsets)
	{
		gapLine = -1;
		gapWidth = 0;
		firstInvalidLineContext = firstInvalidFoldLevel = 0;
		lineCount = endOffsets.getSize();
		this.endOffsets = endOffsets.getArray();
		lineInfo = new int[lineCount];
		for(int i = 0; i < lineInfo.length; i++)
			lineInfo[i] = ((1 << SCREEN_LINES_SHIFT) | VISIBLE_MASK);

		lineContext = new TokenMarker.LineContext[lineCount];

		for(int i = 0; i < positionCount; i++)
			positions[i].offset = 0;
	} 

	
	public void contentInserted(int startLine, int offset,
		int numLines, int length, IntegerArray endOffsets)
	{
		int endLine = startLine + numLines;
		lineInfo[startLine] &= ~SCREEN_LINES_VALID_MASK;

		
		if(numLines > 0)
		{
			

			lineCount += numLines;

			if(this.endOffsets.length <= lineCount)
			{
				int[] endOffsetsN = new int[(lineCount + 1) * 2];
				System.arraycopy(this.endOffsets,0,endOffsetsN,0,
						 this.endOffsets.length);
				this.endOffsets = endOffsetsN;
			}

			if(lineInfo.length <= lineCount)
			{
				int[] lineInfoN = new int[(lineCount + 1) * 2];
				System.arraycopy(lineInfo,0,lineInfoN,0,
						 lineInfo.length);
				lineInfo = lineInfoN;
			}

			if(lineContext.length <= lineCount)
			{
				TokenMarker.LineContext[] lineContextN
					= new TokenMarker.LineContext[(lineCount + 1) * 2];
				System.arraycopy(lineContext,0,lineContextN,0,
						 lineContext.length);
				lineContext = lineContextN;
			}

			System.arraycopy(this.endOffsets,startLine,
				this.endOffsets,endLine,lineCount - endLine);
			System.arraycopy(lineInfo,startLine,lineInfo,
				endLine,lineCount - endLine);
			System.arraycopy(lineContext,startLine,lineContext,
				endLine,lineCount - endLine);

			if(startLine <= gapLine)
				gapLine += numLines;
			else if(gapLine != -1)
				offset -= gapWidth;

			if(startLine < firstInvalidLineContext)
				firstInvalidLineContext += numLines;

			int visible = (lineInfo[startLine] & VISIBLE_MASK);

			for(int i = 0; i < numLines; i++)
			{
				this.endOffsets[startLine + i] = (offset + endOffsets.get(i));
				lineInfo[startLine + i] = visible;
			}


			Anchor anchor = anchors;
			for(;;)
			{
				if(anchor == null)
					break;

				if(anchor.physicalLine < startLine)
					break;

				if(anchor.physicalLine != startLine)
					anchor.physicalLine += numLines;
				anchor.callChanged = true;
				anchor = anchor.next;
			}
		} 

		if(firstInvalidFoldLevel == -1 || firstInvalidFoldLevel > startLine)
			firstInvalidFoldLevel = startLine;
		moveGap(endLine,length,"contentInserted");

		updatePositionsForInsert(offset,length);
	} 

	
	public void contentRemoved(int startLine, int offset,
		int numLines, int length)
	{
		int endLine = startLine + numLines;
		lineInfo[startLine] &= ~SCREEN_LINES_VALID_MASK;

		
		if(numLines > 0)
		{
			moveGap(-1,0,"contentRemoved");

			if(startLine + numLines < gapLine)
				gapLine -= numLines;
			else if(startLine < gapLine)
				gapLine = startLine;

			if(startLine + numLines < firstInvalidLineContext)
				firstInvalidLineContext -= numLines;
			else if(startLine < firstInvalidLineContext)
				firstInvalidLineContext = startLine - 1;

			lineCount -= numLines;

			
			
			Anchor anchor = anchors;
			for(;;)
			{
				if(anchor == null)
					break;

				if(anchor.physicalLine < startLine)
					break;

				if(anchor.physicalLine == startLine)
					anchor.callChanged = true;
				else
				{
					int end = Math.min(endLine,anchor.physicalLine);
					for(int i = startLine; i < end; i++)
					{
						if(isLineVisible(i,anchor.index))
							anchor.scrollLine -= getScreenLineCount(i);
						anchor.physicalLine--;
						anchor.callChanged = true;
					}
				}

				anchor = anchor.next;
			}

			System.arraycopy(endOffsets,endLine,endOffsets,
				startLine,lineCount - startLine);
			System.arraycopy(lineInfo,endLine,lineInfo,
				startLine,lineCount - startLine);
			System.arraycopy(lineContext,endLine,lineContext,
				startLine,lineCount - startLine);
		} 

		if(firstInvalidFoldLevel == -1 || firstInvalidFoldLevel > startLine)
			firstInvalidFoldLevel = startLine;
		moveGap(startLine,-length,"contentRemoved");

		updatePositionsForRemove(offset,length);
	} 

	
	
	public void addAnchor(Anchor anchor)
	{
		Anchor prev = null;
		Anchor current = anchors;
		for(;;)
		{
			if(current == null)
				break;

			if(current.physicalLine < anchor.physicalLine)
			{
				if(prev != null)
					prev.next = anchor;
				else
					anchors = anchor;
				anchor.next = current;
				return;
			}
			prev = current;
			current = current.next;
		}

		if(prev != null)
			prev.next = anchor;
		else
			anchors = anchor;
		anchor.next = null;
	} 

	
	public void removeAnchor(Anchor anchor)
	{
		Anchor current = anchors;
		Anchor prev = null;
		while(current != null)
		{
			if(current == anchor)
			{
				if(prev != null)
					prev.next = current.next;
				else
					anchors = current.next;
				return;
			}
			prev = current;
			current = current.next;
		}
	} 

	
	public void notifyScreenLineChanges()
	{
		Anchor anchor = anchors;
		while(anchor != null)
		{
			if(anchor.callChanged)
			{
				anchor.callChanged = false;
				anchor.changed();
			}
			anchor = anchor.next;
		}
	} 

	

	
	private Buffer buffer;
	private int[] endOffsets;
	private int[] lineInfo;
	private TokenMarker.LineContext[] lineContext;

	private int lineCount;

	private PosBottomHalf[] positions;
	private int positionCount;

	private Anchor anchors;

	
	private int gapLine;
	private int gapWidth;

	
	private int firstInvalidLineContext;

	
	private int firstInvalidFoldLevel;
	

	
	private final void setLineEndOffset(int line, int end)
	{
		endOffsets[line] = end;
	} 

	
	private final void moveGap(int newGapLine, int newGapWidth, String method)
	{
		if(gapLine == -1)
			gapWidth = newGapWidth;
		else if(newGapLine == -1)
		{
			if(gapWidth != 0)
			{
				if(Debug.OFFSET_DEBUG && gapLine != lineCount)
					Log.log(Log.DEBUG,this,method + ": update from " + gapLine + " to " + lineCount + " width " + gapWidth);
				for(int i = gapLine; i < lineCount; i++)
					setLineEndOffset(i,getLineEndOffset(i));
			}

			gapWidth = newGapWidth;
		}
		else if(newGapLine < gapLine)
		{
			if(gapWidth != 0)
			{
				if(Debug.OFFSET_DEBUG && newGapLine != gapLine)
					Log.log(Log.DEBUG,this,method + ": update from " + newGapLine + " to " + gapLine + " width " + gapWidth);
				for(int i = newGapLine; i < gapLine; i++)
					setLineEndOffset(i,getLineEndOffset(i) - gapWidth);
			}
			gapWidth += newGapWidth;
		}
		else 
		{
			if(gapWidth != 0)
			{
				if(Debug.OFFSET_DEBUG && gapLine != newGapLine)
					Log.log(Log.DEBUG,this,method + ": update from " + gapLine + " to " + newGapLine + " width " + gapWidth);
				for(int i = gapLine; i < newGapLine; i++)
					setLineEndOffset(i,getLineEndOffset(i));
			}

			gapWidth += newGapWidth;
		}

		if(newGapLine == lineCount)
			gapLine = -1;
		else
			gapLine = newGapLine;
	} 

	
	private void growPositionArray()
	{
		if(positions.length < positionCount + 1)
		{
			PosBottomHalf[] newPositions = new PosBottomHalf[
				(positionCount + 1) * 2];
			System.arraycopy(positions,0,newPositions,0,positionCount);
			positions = newPositions;
		}
	} 

	
	private synchronized void removePosition(PosBottomHalf bh)
	{
		int index = -1;

		for(int i = 0; i < positionCount; i++)
		{
			if(positions[i] == bh)
			{
				index = i;
				break;
			}
		}

		System.arraycopy(positions,index + 1,positions,index,
			positionCount - index - 1);
		positions[--positionCount] = null;
	} 

	
	private void updatePositionsForInsert(int offset, int length)
	{
		if(positionCount == 0)
			return;

		int start = getPositionAtOffset(offset);

		for(int i = start; i < positionCount; i++)
		{
			PosBottomHalf bh = positions[i];
			if(bh.offset < offset)
				Log.log(Log.ERROR,this,"Screwed up: " + bh.offset);
			else
				bh.offset += length;
		}
	} 

	
	private void updatePositionsForRemove(int offset, int length)
	{
		if(positionCount == 0)
			return;

		int start = getPositionAtOffset(offset);

		for(int i = start; i < positionCount; i++)
		{
			PosBottomHalf bh = positions[i];
			if(bh.offset < offset)
				Log.log(Log.ERROR,this,"Screwed up: " + bh.offset);
			else if(bh.offset < offset + length)
				bh.offset = offset;
			else
				bh.offset -= length;
		}
	} 

	
	private int getPositionAtOffset(int offset)
	{
		int start = 0;
		int end = positionCount - 1;

		PosBottomHalf bh;

loop:		for(;;)
		{
			switch(end - start)
			{
			case 0:
				bh = positions[start];
				if(bh.offset < offset)
					start++;
				break loop;
			case 1:
				bh = positions[end];
				if(bh.offset < offset)
				{
					start = end + 1;
				}
				else
				{
					bh = positions[start];
					if(bh.offset < offset)
					{
						start++;
					}
				}
				break loop;
			default:
				int pivot = (start + end) / 2;
				bh = positions[pivot];
				if(bh.offset > offset)
					end = pivot - 1;
				else
					start = pivot + 1;
				break;
			}
		}

		return start;
	} 

	

	

	
	static class PosTopHalf implements Position
	{
		PosBottomHalf bh;

		
		PosTopHalf(PosBottomHalf bh)
		{
			this.bh = bh;
			bh.ref();
		} 

		
		public int getOffset()
		{
			return bh.offset;
		} 

		
		public void finalize()
		{
			bh.unref();
		} 
	} 

	
	class PosBottomHalf
	{
		int offset;
		int ref;

		
		PosBottomHalf(int offset)
		{
			this.offset = offset;
		} 

		
		void ref()
		{
			ref++;
		} 

		
		void unref()
		{
			if(--ref == 0)
				removePosition(this);
		} 
	} 

	
	
	public static abstract class Anchor
	{
		public Anchor(int index)
		{
			this.index = index;
		}

		public Anchor next;

		public int physicalLine;
		public int scrollLine;
		public int index;
		public boolean callChanged;

		public abstract void reset();
		public abstract void changed();
	} 

	
}
