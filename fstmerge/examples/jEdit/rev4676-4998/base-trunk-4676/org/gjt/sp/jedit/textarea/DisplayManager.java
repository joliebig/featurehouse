

package org.gjt.sp.jedit.textarea;


import javax.swing.SwingUtilities;
import java.awt.Toolkit;
import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.textarea.JEditTextArea;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class DisplayManager
{
	
	
	public final boolean isLineVisible(int line)
	{
		return offsetMgr.isLineVisible(line,index);
	} 

	public static long scanCount, scannedLines;

	
	
	public int getFirstVisibleLine()
	{
		scanCount++;
		try
		{
			buffer.readLock();

			for(int i = 0; i < buffer.getLineCount(); i++)
			{
				if(isLineVisible(i))
				{
					scannedLines += i + 1;
					return i;
				}
			}
		}
		finally
		{
			buffer.readUnlock();
		}

		
		return -1;
	} 

	
	
	public int getLastVisibleLine()
	{
		scanCount++;

		try
		{
			buffer.readLock();

			for(int i = buffer.getLineCount() - 1; i >= 0; i--)
			{
				if(isLineVisible(i))
				{
					scannedLines += (buffer.getLineCount() - i);
					return i;
				}
			}
		}
		finally
		{
			buffer.readUnlock();
		}

		
		return -1;
	} 

	
	
	public int getNextVisibleLine(int line)
	{
		if(line < 0 || line >= offsetMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		scanCount++;

		try
		{
			buffer.readLock();

			if(line == buffer.getLineCount() - 1)
				return -1;

			for(int i = line + 1; i < buffer.getLineCount(); i++)
			{
				if(isLineVisible(i))
				{
					scannedLines += (i - line);
					return i;
				}
			}
			return -1;
		}
		finally
		{
			buffer.readUnlock();
		}
	} 

	
	
	public int getPrevVisibleLine(int line)
	{
		if(line < 0 || line >= offsetMgr.getLineCount())
			throw new ArrayIndexOutOfBoundsException(line);

		scanCount++;

		try
		{
			buffer.readLock();

			if(line == 0)
				return -1;

			for(int i = line - 1; i >= 0; i--)
			{
				if(isLineVisible(i))
				{
					scannedLines += (line - i);
					return i;
				}
			}
			return -1;
		}
		finally
		{
			buffer.readUnlock();
		}
	} 

	
	public final int getScreenLineCount(int line)
	{
		if(offsetMgr.isScreenLineCountValid(line))
			return offsetMgr.getScreenLineCount(line);
		else
		{
			int newCount = textArea.chunkCache.getLineInfosForPhysicalLine(line).length;

			offsetMgr.setScreenLineCount(line,newCount);
			return newCount;
		}
	} 

	
	public final int getScrollLineCount()
	{
		return scrollLineCount.scrollLine;
	} 

	
	
	public void collapseFold(int line)
	{
		int lineCount = buffer.getLineCount();
		int start = 0;
		int end = lineCount - 1;

		try
		{
			buffer.writeLock();

			
			
			if(line != 0
				&& line != buffer.getLineCount() - 1
				&& buffer.isFoldStart(line)
				&& !isLineVisible(line + 1))
			{
				line--;
			}

			int initialFoldLevel = buffer.getFoldLevel(line);

			
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
		}
		finally
		{
			buffer.writeUnlock();
		}

		offsetMgr.notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	
	public int expandFold(int line, boolean fully)
	{
		
		int returnValue = -1;

		int lineCount = buffer.getLineCount();
		int start = 0;
		int end = lineCount - 1;

		try
		{
			buffer.writeLock();

			int initialFoldLevel = buffer.getFoldLevel(line);

			
			if(line != lineCount - 1
				&& isLineVisible(line)
				&& !isLineVisible(line + 1)
				&& buffer.getFoldLevel(line + 1) > initialFoldLevel)
			{
				
				start = line + 1;

				for(int i = line + 1; i < lineCount; i++)
				{
					if(
						buffer.getFoldLevel(i) <= initialFoldLevel)
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
					if(isLineVisible(i) && buffer.getFoldLevel(i) < initialFoldLevel)
					{
						start = i + 1;
						ok = true;
						break;
					}
				}

				if(!ok)
				{
					
					return -1;
				}

				for(int i = line + 1; i < lineCount; i++)
				{
					if((isLineVisible(i) &&
						buffer.getFoldLevel(i) < initialFoldLevel)
						|| i == getLastVisibleLine())
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
				
				initialFoldLevel = buffer.getFoldLevel(start);

				int firstVisible = start;

				for(int i = start; i <= end; i++)
				{
					if(buffer.getFoldLevel(i) > initialFoldLevel)
					{
						if(returnValue == -1
							&& i != 0
							&& buffer.isFoldStart(i - 1))
						{
							returnValue = i - 1;
						}

						if(firstVisible != i)
						{
							showLineRange(firstVisible,i - 1);
						}
						firstVisible = i + 1;
					}
				}

				if(firstVisible != end + 1)
					showLineRange(firstVisible,end);

				if(!isLineVisible(line))
				{
					
					expandFold(line,false);
					return returnValue;
				}
			} 
		}
		finally
		{
			buffer.writeUnlock();
		}

		offsetMgr.notifyScreenLineChanges();
		textArea.foldStructureChanged();

		return returnValue;
	} 

	
	
	public void expandAllFolds()
	{
		try
		{
			buffer.writeLock();

			showLineRange(0,buffer.getLineCount() - 1);
		}
		finally
		{
			buffer.writeUnlock();
		}

		offsetMgr.notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	
	public void expandFolds(char digit)
	{
		if(digit < '1' || digit > '9')
		{
			Toolkit.getDefaultToolkit().beep();
			return;
		}
		else
			expandFolds((int)(digit - '1') + 1);
	} 

	
	
	public void expandFolds(int foldLevel)
	{
		try
		{
			buffer.writeLock();

			if(buffer.getFoldHandler() instanceof IndentFoldHandler)
				foldLevel = (foldLevel - 1) * buffer.getIndentSize() + 1;

			
			boolean seenVisibleLine = false;

			for(int i = 0; i < buffer.getLineCount(); i++)
			{
				if(!seenVisibleLine || buffer.getFoldLevel(i) < foldLevel)
				{
					seenVisibleLine = true;
					setLineVisible(i,true);
				}
				else
					setLineVisible(i,false);
			}
		}
		finally
		{
			buffer.writeUnlock();
		}

		offsetMgr.notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	
	public void narrow(int start, int end)
	{
		if(start > end || start < 0 || end >= buffer.getLineCount())
			throw new ArrayIndexOutOfBoundsException(start + ", " + end);

		if(start < getFirstVisibleLine() || end > getLastVisibleLine())
			expandAllFolds();

		hideLineRange(0,start - 1);
		hideLineRange(end + 1,buffer.getLineCount() - 1);

		
		if(getNextVisibleLine(start) == -1)
			expandFold(start,false);

		
		
		GUIUtilities.getView(textArea).getStatus().setMessageAndClear(
			jEdit.getProperty("view.status.narrow"));

		offsetMgr.notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	boolean softWrap;
	int wrapMargin;
	FirstLine firstLine;
	ScrollLineCount scrollLineCount;

	
	DisplayManager(Buffer buffer, JEditTextArea textArea)
	{
		this.buffer = buffer;
		this.offsetMgr = buffer._getOffsetManager();
		this.textArea = textArea;
		this.index = buffer._displayLock();

		scrollLineCount = new ScrollLineCount(index);
		offsetMgr.addAnchor(scrollLineCount);

		firstLine = new FirstLine(index);
		offsetMgr.addAnchor(firstLine);
	} 

	
	void dispose()
	{
		offsetMgr.removeAnchor(scrollLineCount);
		offsetMgr.removeAnchor(firstLine);
		offsetMgr = null;

		buffer._displayUnlock(index);
		buffer = null;
	} 

	
	void notifyScreenLineChanges()
	{
		offsetMgr.notifyScreenLineChanges();
	} 

	
	
	public void setScreenLineCount(int line, int count)
	{
		try
		{
			buffer.writeLock();
			offsetMgr.setScreenLineCount(line,count);
		}
		finally
		{
			buffer.writeUnlock();
		}
	} 

	
	void updateWrapSettings()
	{
		String wrap = buffer.getStringProperty("wrap");
		softWrap = wrap.equals("soft");
		if(textArea.maxLineLen <= 0)
		{
			softWrap = false;
			wrapMargin = 0;
		}
		else
		{
			
			char[] foo = new char[textArea.maxLineLen];
			for(int i = 0; i < foo.length; i++)
			{
				foo[i] = ' ';
			}
			TextAreaPainter painter = textArea.getPainter();
			wrapMargin = (int)painter.getFont().getStringBounds(
				foo,0,foo.length,
				painter.getFontRenderContext())
				.getWidth();
		}
	} 

	

	
	private Buffer buffer;
	private OffsetManager offsetMgr;
	private JEditTextArea textArea;
	private int index;

	
	private final void setLineVisible(int line, boolean visible)
	{
		offsetMgr.setLineVisible(line,index,visible);
	} 

	
	private void showLineRange(int start, int end)
	{
		for(int i = start; i <= end; i++)
		{
			offsetMgr.setLineVisible(i,index,true);
		}
	} 

	
	private void hideLineRange(int start, int end)
	{
		for(int i = start; i <= end; i++)
		{
			offsetMgr.setLineVisible(i,index,false);
		}
	} 

	

	
	class ScrollLineCount extends OffsetManager.Anchor
	{
		
		ScrollLineCount(int index)
		{
			super(index);
		} 

		
		public void changed()
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"changed()");
			textArea.updateScrollBars();
			textArea.recalculateLastPhysicalLine();
		} 

		
		public void reset()
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"reset()");

			updateWrapSettings();

			offsetMgr.removeAnchor(this);
			physicalLine = offsetMgr.getLineCount();
			scrollLine = 0;
			for(int i = 0; i < physicalLine; i++)
			{
				if(isLineVisible(i))
					scrollLine += getScreenLineCount(i);
			}
			offsetMgr.addAnchor(this);

			firstLine.ensurePhysicalLineIsVisible();

			textArea.recalculateLastPhysicalLine();
			textArea.updateScrollBars();
		} 
	} 

	
	class FirstLine extends OffsetManager.Anchor
	{
		int skew;

		
		FirstLine(int index)
		{
			super(index);
		} 

		
		public void changed()
		{
			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"changed() before: "
					+ physicalLine + ":" + scrollLine);
			}

			ensurePhysicalLineIsVisible();

			int screenLines = getScreenLineCount(physicalLine);
			if(skew >= screenLines)
				skew = screenLines - 1;

			if(Debug.VERIFY_FIRST_LINE)
			{
				int verifyScrollLine = 0;

				for(int i = 0; i < buffer.getLineCount(); i++)
				{
					if(!isLineVisible(i))
						continue;
	
					if(i >= physicalLine)
						break;
	
					verifyScrollLine += getScreenLineCount(i);
				}

				if(verifyScrollLine != scrollLine)
				{
					Exception ex = new Exception(scrollLine + ":" + verifyScrollLine);
					Log.log(Log.ERROR,this,ex);
					new org.gjt.sp.jedit.gui.BeanShellErrorDialog(null,ex);
				}
			}

			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"changed() after: "
					+ physicalLine + ":" + scrollLine);
			}

			textArea.updateScrollBars();
			textArea.recalculateLastPhysicalLine();
		} 

		
		public void reset()
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"reset()");

			String wrap = buffer.getStringProperty("wrap");
			softWrap = wrap.equals("soft");
			if(textArea.maxLineLen <= 0)
			{
				softWrap = false;
				wrapMargin = 0;
			}
			else
			{
				
				char[] foo = new char[textArea.maxLineLen];
				for(int i = 0; i < foo.length; i++)
				{
					foo[i] = ' ';
				}
				TextAreaPainter painter = textArea.getPainter();
				wrapMargin = (int)painter.getFont().getStringBounds(
					foo,0,foo.length,
					painter.getFontRenderContext())
					.getWidth();
			}

			scrollLine = 0;

			int i = 0;

			for(; i < buffer.getLineCount(); i++)
			{
				if(!isLineVisible(i))
					continue;

				if(i >= physicalLine)
					break;

				scrollLine += getScreenLineCount(i);
			}

			physicalLine = i;

			int screenLines = getScreenLineCount(physicalLine);
			if(skew >= screenLines)
				skew = screenLines - 1;

			textArea.updateScrollBars();
		} 

		
		
		void physDown(int amount, int screenAmount)
		{
			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"physDown() start: "
					+ physicalLine + ":" + scrollLine);
			}

			skew = 0;

			offsetMgr.removeAnchor(this);

			if(!isLineVisible(physicalLine))
			{
				int lastVisibleLine = getLastVisibleLine();
				if(physicalLine > lastVisibleLine)
					physicalLine = lastVisibleLine;
				else
				{
					int nextPhysicalLine = getNextVisibleLine(physicalLine);
					amount -= (nextPhysicalLine - physicalLine);
					physicalLine = nextPhysicalLine;
					scrollLine += getScreenLineCount(physicalLine);
				}
			}

			for(;;)
			{
				int nextPhysicalLine = getNextVisibleLine(
					physicalLine);
				if(nextPhysicalLine == -1)
					break;
				else if(nextPhysicalLine > physicalLine + amount)
					break;
				else
				{
					scrollLine += getScreenLineCount(physicalLine);
					amount -= (nextPhysicalLine - physicalLine);
					physicalLine = nextPhysicalLine;
				}
			}

			offsetMgr.addAnchor(this);

			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"physDown() end: "
					+ physicalLine + ":" + scrollLine);
			}

			
			
			if(screenAmount != 0)
			{
				if(screenAmount < 0)
					scrollUp(-screenAmount);
				else
					scrollDown(screenAmount);
			}
		} 

		
		
		void physUp(int amount, int screenAmount)
		{
			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"physUp() start: "
					+ physicalLine + ":" + scrollLine);
			}

			skew = 0;

			offsetMgr.removeAnchor(this);

			if(!isLineVisible(physicalLine))
			{
				int firstVisibleLine = getFirstVisibleLine();
				if(physicalLine < firstVisibleLine)
					physicalLine = firstVisibleLine;
				else
				{
					int prevPhysicalLine = getPrevVisibleLine(physicalLine);
					amount -= (physicalLine - prevPhysicalLine);
				}
			}

			for(;;)
			{
				int prevPhysicalLine = getPrevVisibleLine(
					physicalLine);
				if(prevPhysicalLine == -1)
					break;
				else if(prevPhysicalLine < physicalLine - amount)
					break;
				else
				{
					scrollLine -= getScreenLineCount(
						prevPhysicalLine);
					amount -= (physicalLine - prevPhysicalLine);
					physicalLine = prevPhysicalLine;
				}
			}

			offsetMgr.addAnchor(this);

			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"physUp() end: "
					+ physicalLine + ":" + scrollLine);
			}

			
			
			if(screenAmount != 0)
			{
				if(screenAmount < 0)
					scrollUp(-screenAmount);
				else
					scrollDown(screenAmount);
			}
		} 

		
		
		void scrollDown(int amount)
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"scrollDown()");

			ensurePhysicalLineIsVisible();

			offsetMgr.removeAnchor(this);

			amount += skew;

			skew = 0;

			while(amount > 0)
			{
				int screenLines = getScreenLineCount(physicalLine);
				if(amount < screenLines)
				{
					skew = amount;
					break;
				}
				else
				{
					int nextLine = getNextVisibleLine(physicalLine);
					if(nextLine == -1)
						break;
					boolean visible = isLineVisible(physicalLine);
					physicalLine = nextLine;
					if(visible)
					{
						amount -= screenLines;
						scrollLine += screenLines;
					}
				}
			}

			offsetMgr.addAnchor(this);
		} 

		
		
		void scrollUp(int amount)
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"scrollUp()");

			ensurePhysicalLineIsVisible();

			offsetMgr.removeAnchor(this);

			if(amount <= skew)
			{
				skew -= amount;
			}
			else
			{
				amount -= skew;
				skew = 0;

				while(amount > 0)
				{
					int prevLine = getPrevVisibleLine(physicalLine);
					if(prevLine == -1)
						break;
					physicalLine = prevLine;

					int screenLines = getScreenLineCount(physicalLine);
					scrollLine -= screenLines;
					if(amount < screenLines)
					{
						skew = screenLines - amount;
						break;
					}
					else
						amount -= screenLines;
				}
			}

			offsetMgr.addAnchor(this);
		} 

		
		private void ensurePhysicalLineIsVisible()
		{
			if(!isLineVisible(physicalLine))
			{
				if(physicalLine > getLastVisibleLine())
				{
					physicalLine = getPrevVisibleLine(physicalLine);
					scrollLine -= getScreenLineCount(physicalLine);
				}
				else
				{
					physicalLine = getNextVisibleLine(physicalLine);
					scrollLine += getScreenLineCount(physicalLine);
				}
			}
		} 
	} 
}
