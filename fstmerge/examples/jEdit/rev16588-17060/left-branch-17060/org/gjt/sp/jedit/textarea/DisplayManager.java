

package org.gjt.sp.jedit.textarea;


import java.awt.Toolkit;
import java.util.*;
import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.Debug;
import org.gjt.sp.util.Log;



public class DisplayManager
{
	

	
	static DisplayManager getDisplayManager(JEditBuffer buffer,
		TextArea textArea)
	{
		List<DisplayManager> l = bufferMap.get(buffer);
		if(l == null)
		{
			l = new LinkedList<DisplayManager>();
			bufferMap.put(buffer,l);
		}

		
		DisplayManager copy = null;
		Iterator<DisplayManager> liter = l.iterator();
		DisplayManager dmgr;
		while(liter.hasNext())
		{
			dmgr = liter.next();
			copy = dmgr;
			if(!dmgr.inUse && dmgr.textArea == textArea)
			{
				dmgr.inUse = true;
				return dmgr;
			}
		}

		
		dmgr = new DisplayManager(buffer,textArea,copy);
		dmgr.inUse = true;
		l.add(dmgr);

		return dmgr;
	} 

	
	void release()
	{
		inUse = false;
	} 

	
	public static void bufferClosed(JEditBuffer buffer)
	{
		bufferMap.remove(buffer);
	} 

	
	static void textAreaDisposed(TextArea textArea)
	{
		for (List<DisplayManager> l : bufferMap.values())
		{
			Iterator<DisplayManager> liter = l.iterator();
			while(liter.hasNext())
			{
				DisplayManager dmgr = liter.next();
				if(dmgr.textArea == textArea)
				{
					dmgr.dispose();
					liter.remove();
				}
			}
		}
	} 

	private static final Map<JEditBuffer, List<DisplayManager>> bufferMap = new HashMap<JEditBuffer, List<DisplayManager>>();
	

	
	
	public JEditBuffer getBuffer()
	{
		return buffer;
	} 

	
	
	public final boolean isLineVisible(int line)
	{
		return folds.search(line) % 2 == 0;
	} 

	
	
	public int getFirstVisibleLine()
	{
		return folds.first();
	} 

	
	
	public int getLastVisibleLine()
	{
		return folds.last();
	} 

	
	
	public int getNextVisibleLine(int line)
	{
		if(line < 0 || line >= buffer.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		return folds.next(line);
	} 

	
	
	public int getPrevVisibleLine(int line)
	{
		if(line < 0 || line >= buffer.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		return folds.prev(line);
	} 

	
	
	public final int getScreenLineCount(int line)
	{
		updateScreenLineCount(line);
		return screenLineMgr.getScreenLineCount(line);
	} 

	
	
	public final int getScrollLineCount()
	{
		return scrollLineCount.scrollLine;
	} 

	
	
	public void collapseFold(int line)
	{
		int lineCount = buffer.getLineCount();
		int end = lineCount - 1;

		
		
		if(line != 0
			&& line != buffer.getLineCount() - 1
			&& buffer.isFoldStart(line)
			&& !isLineVisible(line + 1))
		{
			line--;
		}

		int initialFoldLevel = buffer.getFoldLevel(line);

		
		int start = 0;
		if(line != lineCount - 1
			&& buffer.getFoldLevel(line + 1) > initialFoldLevel)
		{
			
			start = line + 1;

			for(int i = line + 1; i < lineCount; i++)
			{
				if(buffer.getFoldLevel(i) <= initialFoldLevel)
				{
					end = i - 1;
					break;
				}
			}
		}
		else
		{
			boolean ok = false;

			
			for(int i = line - 1; i >= 0; i--)
			{
				if(buffer.getFoldLevel(i) < initialFoldLevel)
				{
					start = i + 1;
					ok = true;
					break;
				}
			}

			if(!ok)
			{
				
				return;
			}

			for(int i = line + 1; i < lineCount; i++)
			{
				if(buffer.getFoldLevel(i) < initialFoldLevel)
				{
					end = i - 1;
					break;
				}
			}
		} 

		
		hideLineRange(start,end);

		notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	
	public int expandFold(int line, boolean fully)
	{
		
		int returnValue = -1;

		int lineCount = buffer.getLineCount();
		int end = lineCount - 1;

		if (line == lineCount - 1)
		{
			return -1;
		}
		while (!isLineVisible(line))
		{
			int prevLine = folds.lookup(folds.search(line)) - 1;
			if (!isLineVisible(prevLine))
			{
				return -1;
			}
			expandFold(prevLine, fully);
			if (!isLineVisible(prevLine + 1))
			{
				return -1;
			}
		}
		if (isLineVisible(line+1) && !fully)
		{
			return -1;
		}

		
		int start;
		int initialFoldLevel = buffer.getFoldLevel(line);
		if (buffer.getFoldLevel(line + 1) > initialFoldLevel)
		{
			
			start = line;
			if (!isLineVisible(line + 1) && folds.search(line + 1) != folds.count() - 1)
			{
				int index = folds.search(line + 1);
				end = folds.lookup(index + 1) - 1;
			}
			else
			{
				for (int i = line + 1; i < lineCount; i++)
				{
					if (buffer.getFoldLevel(i) <= initialFoldLevel)
					{
						end = i - 1;
						break;
					}
				}
			}
		}
		else
		{
			if (!fully)
			{
				return -1;
			}
			start = line;
			while (start > 0 && buffer.getFoldLevel(start) >= initialFoldLevel)
			{
				start--;
			}
			initialFoldLevel = buffer.getFoldLevel(start);
			for (int i = line + 1; i < lineCount; i++)
			{
				if (buffer.getFoldLevel(i) <= initialFoldLevel)
				{
					end = i - 1;
					break;
				}
			}
		} 

		
		if(fully)
		{
			showLineRange(start,end);
		}
		else
		{
			for (int i = start + 1; i <= end;)
			{
				if (returnValue == -1 && buffer.isFoldStart(i))
				{
					returnValue = i;
				}

				showLineRange(i, i);
				int fold = buffer.getFoldLevel(i);
				i++;
				while (i <= end && buffer.getFoldLevel(i) > fold)
				{
					i++;
				}
			}
		} 

		notifyScreenLineChanges();
		textArea.foldStructureChanged();

		return returnValue;
	} 

	
	
	public void expandAllFolds()
	{
		showLineRange(0,buffer.getLineCount() - 1);
		notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	
	public void expandFolds(char digit)
	{
		if(digit < '1' || digit > '9')
		{
			Toolkit.getDefaultToolkit().beep();
		}
		else
			expandFolds((digit - '1') + 1);
	} 

	
	
	public void expandFolds(int foldLevel)
	{
		if(buffer.getFoldHandler() instanceof IndentFoldHandler)
			foldLevel = (foldLevel - 1) * buffer.getIndentSize() + 1;

		showLineRange(0,buffer.getLineCount() - 1);

		
		boolean seenVisibleLine = false;

		int firstInvisible = 0;

		for(int i = 0; i < buffer.getLineCount(); i++)
		{
			if(!seenVisibleLine || buffer.getFoldLevel(i) < foldLevel)
			{
				if(firstInvisible != i)
				{
					hideLineRange(firstInvisible,
						i - 1);
				}
				firstInvisible = i + 1;
				seenVisibleLine = true;
			}
		}

		if(firstInvisible != buffer.getLineCount())
			hideLineRange(firstInvisible,buffer.getLineCount() - 1);

		notifyScreenLineChanges();
		if(textArea.getDisplayManager() == this)
		{
			textArea.foldStructureChanged();
		}
	} 

	
	
	public void narrow(int start, int end)
	{
		if(start > end || start < 0 || end >= buffer.getLineCount())
			throw new ArrayIndexOutOfBoundsException(start + ", " + end);

		if(start < getFirstVisibleLine() || end > getLastVisibleLine())
			expandAllFolds();

		if(start != 0)
			hideLineRange(0,start - 1);
		if(end != buffer.getLineCount() - 1)
			hideLineRange(end + 1,buffer.getLineCount() - 1);

		
		if(start != buffer.getLineCount() - 1
			&& !isLineVisible(start + 1))
			expandFold(start,false);

		textArea.fireNarrowActive();

		notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	final FirstLine firstLine;
	final ScrollLineCount scrollLineCount;
	final ScreenLineManager screenLineMgr;
	RangeMap folds;

	
	void init()
	{
		if(initialized)
		{
			if(!buffer.isLoading())
				resetAnchors();
		}
		else
		{
			initialized = true;
			folds = new RangeMap();
			if(buffer.isLoading())
				folds.reset(buffer.getLineCount());
			else
				bufferHandler.foldHandlerChanged(buffer);
			notifyScreenLineChanges();
		}
	} 

	
	void notifyScreenLineChanges()
	{
		if(Debug.SCROLL_DEBUG)
			Log.log(Log.DEBUG,this,"notifyScreenLineChanges()");

		
		
		if(textArea.getDisplayManager() != this)
			return;

		try
		{
			if(firstLine.callReset)
				firstLine.reset();
			else if(firstLine.callChanged)
				firstLine.changed();

			if(scrollLineCount.callReset)
			{
				scrollLineCount.reset();
				firstLine.ensurePhysicalLineIsVisible();
			}
			else if(scrollLineCount.callChanged)
				scrollLineCount.changed();
			
			if(firstLine.callChanged || scrollLineCount.callReset
				|| scrollLineCount.callChanged)
			{
				textArea.updateScrollBar();
				textArea.recalculateLastPhysicalLine();
			}
		}
		finally
		{
			firstLine.callReset = firstLine.callChanged = false;
			scrollLineCount.callReset = scrollLineCount.callChanged = false;
		}
	} 

	
	void setFirstLine(int oldFirstLine, int firstLine)
	{
		int visibleLines = textArea.getVisibleLines();

		if(firstLine >= oldFirstLine + visibleLines)
		{
			this.firstLine.scrollDown(firstLine - oldFirstLine);
			textArea.chunkCache.invalidateAll();
		}
		else if(firstLine <= oldFirstLine - visibleLines)
		{
			this.firstLine.scrollUp(oldFirstLine - firstLine);
			textArea.chunkCache.invalidateAll();
		}
		else if(firstLine > oldFirstLine)
		{
			this.firstLine.scrollDown(firstLine - oldFirstLine);
			textArea.chunkCache.scrollDown(firstLine - oldFirstLine);
		}
		else if(firstLine < oldFirstLine)
		{
			this.firstLine.scrollUp(oldFirstLine - firstLine);
			textArea.chunkCache.scrollUp(oldFirstLine - firstLine);
		}

		notifyScreenLineChanges();
	} 

	
	
	void setFirstPhysicalLine(int amount, int skew)
	{
		int oldFirstLine = textArea.getFirstLine();

		if(amount == 0)
		{
			skew -= this.firstLine.skew;

			
			
			if(skew < 0)
				this.firstLine.scrollUp(-skew);
			else if(skew > 0)
				this.firstLine.scrollDown(skew);
			else
			{
				
				return;
			}
		}
		else if(amount > 0)
			this.firstLine.physDown(amount,skew);
		else if(amount < 0)
			this.firstLine.physUp(-amount,skew);

		int firstLine = textArea.getFirstLine();
		int visibleLines = textArea.getVisibleLines();

		if(firstLine == oldFirstLine)
			;
		else if(firstLine >= oldFirstLine + visibleLines
			|| firstLine <= oldFirstLine - visibleLines)
		{
			textArea.chunkCache.invalidateAll();
		}
		else if(firstLine > oldFirstLine)
		{
			textArea.chunkCache.scrollDown(firstLine - oldFirstLine);
		}
		else if(firstLine < oldFirstLine)
		{
			textArea.chunkCache.scrollUp(oldFirstLine - firstLine);
		}

		
		notifyScreenLineChanges();
	} 

	
	void invalidateScreenLineCounts()
	{
		screenLineMgr.invalidateScreenLineCounts();
		firstLine.callReset = true;
		scrollLineCount.callReset = true;
	} 

	
	void updateScreenLineCount(int line)
	{
		if(!screenLineMgr.isScreenLineCountValid(line))
		{
			int newCount = textArea.chunkCache
				.getLineSubregionCount(line);

			setScreenLineCount(line,newCount);
		}
	} 

	
	void bufferLoaded()
	{
		folds.reset(buffer.getLineCount());
		screenLineMgr.reset();

		if(textArea.getDisplayManager() == this)
		{
			textArea.propertiesChanged();
			init();
		}

		int collapseFolds = buffer.getIntegerProperty(
			"collapseFolds",0);
		if(collapseFolds != 0)
			expandFolds(collapseFolds);
	} 

	
	void foldHandlerChanged()
	{
		if(buffer.isLoading())
			return;

		folds.reset(buffer.getLineCount());
		resetAnchors();

		int collapseFolds = buffer.getIntegerProperty(
			"collapseFolds",0);
		if(collapseFolds != 0)
			expandFolds(collapseFolds);
	} 

	

	
	private boolean initialized;
	private boolean inUse;
	private final JEditBuffer buffer;
	private final TextArea textArea;
	private final BufferHandler bufferHandler;

	
	private DisplayManager(JEditBuffer buffer, TextArea textArea,
		DisplayManager copy)
	{
		this.buffer = buffer;
		this.screenLineMgr = new ScreenLineManager(buffer);
		this.textArea = textArea;

		scrollLineCount = new ScrollLineCount(this,textArea);
		firstLine = new FirstLine(this,textArea);

		bufferHandler = new BufferHandler(this,textArea,buffer);
		
		buffer.addBufferListener(bufferHandler, JEditBuffer.HIGH_PRIORITY);

		if(copy != null)
		{
			folds = new RangeMap(copy.folds);
			initialized = true;
		}
	} 

	
	private void resetAnchors()
	{
		firstLine.callReset = true;
		scrollLineCount.callReset = true;
		notifyScreenLineChanges();
	} 

	
	private void dispose()
	{
		buffer.removeBufferListener(bufferHandler);
	} 

	
	private void showLineRange(int start, int end)
	{
		if(Debug.FOLD_VIS_DEBUG)
		{
			Log.log(Log.DEBUG,this,"showLineRange(" + start
				+ ',' + end + ')');
		}

		for(int i = start; i <= end; i++)
		{
			
			if(!isLineVisible(i))
			{
				
				int screenLines = getScreenLineCount(i);
				if(firstLine.physicalLine >= i)
				{
					firstLine.scrollLine += screenLines;
					firstLine.callChanged = true;
				}
				scrollLineCount.scrollLine += screenLines;
				scrollLineCount.callChanged = true;
			}
		}

		
		folds.show(start,end);
	} 

	
	private void hideLineRange(int start, int end)
	{
		if(Debug.FOLD_VIS_DEBUG)
		{
			Log.log(Log.DEBUG,this,"hideLineRange(" + start
				+ ',' + end + ')');
		}

		int i = start;
		if(!isLineVisible(i))
			i = getNextVisibleLine(i);
		while(i != -1 && i <= end)
		{
			int screenLines = getScreenLineCount(i);
			if(i < firstLine.physicalLine)
			{
				firstLine.scrollLine -= screenLines;
				firstLine.skew = 0;
				firstLine.callChanged = true;
			}

			scrollLineCount.scrollLine -= screenLines;
			scrollLineCount.callChanged = true;

			i = getNextVisibleLine(i);
		}

		
		folds.hide(start,end);

		if(!isLineVisible(firstLine.physicalLine))
		{
			int firstVisible = getFirstVisibleLine();
			if(firstLine.physicalLine < firstVisible)
			{
				firstLine.physicalLine = firstVisible;
				firstLine.scrollLine = 0;
			}
			else
			{
				firstLine.physicalLine = getPrevVisibleLine(
					firstLine.physicalLine);
				firstLine.scrollLine -= getScreenLineCount(
					firstLine.physicalLine);
			}
			firstLine.callChanged = true;
		}
	} 

	
	
	private void setScreenLineCount(int line, int count)
	{
		int oldCount = screenLineMgr.getScreenLineCount(line);

		
		

		screenLineMgr.setScreenLineCount(line,count);

		if(count == oldCount)
			return;

		if(!isLineVisible(line))
			return;

		if(firstLine.physicalLine >= line)
		{
			if(firstLine.physicalLine == line)
				firstLine.callChanged = true;
			else
			{
				firstLine.scrollLine += count - oldCount;
				firstLine.callChanged = true;
			}
		}

		scrollLineCount.scrollLine += count - oldCount;
		scrollLineCount.callChanged = true;
	} 

	
}
