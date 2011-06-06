

package org.gjt.sp.jedit.textarea;


import java.awt.Toolkit;
import java.util.*;
import org.gjt.sp.jedit.buffer.*;
import org.gjt.sp.jedit.*;
import org.gjt.sp.util.Log;



public class DisplayManager
{
	

	public static long scanCount, scannedLines;

	
	static DisplayManager getDisplayManager(Buffer buffer,
		JEditTextArea textArea)
	{
		List l = (List)bufferMap.get(buffer);
		DisplayManager dmgr;
		if(l == null)
		{
			l = new LinkedList();
			bufferMap.put(buffer,l);
		}

		Iterator liter = l.iterator();
		while(liter.hasNext())
		{
			dmgr = (DisplayManager)liter.next();
			if(!dmgr.inUse && dmgr.textArea == textArea)
			{
				dmgr.inUse = true;
				return dmgr;
			}
		}

		
		dmgr = new DisplayManager(buffer,textArea);
		dmgr.inUse = true;
		l.add(dmgr);

		return dmgr;
	} 

	
	static void releaseDisplayManager(DisplayManager dmgr)
	{
		dmgr.inUse = false;
	} 

	
	public static void bufferClosed(Buffer buffer)
	{
		bufferMap.remove(buffer);
	} 

	
	static void textAreaDisposed(JEditTextArea textArea)
	{
		Iterator biter = bufferMap.values().iterator();
		while(biter.hasNext())
		{
			List l = (List)biter.next();
			Iterator liter = l.iterator();
			while(liter.hasNext())
			{
				DisplayManager dmgr = (DisplayManager)
					liter.next();
				if(dmgr.textArea == textArea)
				{
					dmgr.dispose();
					liter.remove();
				}
			}
		}
	} 

	
	 

	private static Map bufferMap = new HashMap();
	

	
	
	public final boolean isLineVisible(int line)
	{
		return fvmget(line) % 2 == 0;
	} 

	
	
	public int getFirstVisibleLine()
	{
		return fvm[0];
	} 

	
	
	public int getLastVisibleLine()
	{
		return fvm[fvmcount - 1] - 1;
	} 

	
	
	public int getNextVisibleLine(int line)
	{
		int index = fvmget(line);
		
		if(index % 2 != 0)
		{
			
			if(fvmcount == index + 1)
				return - 1;
			
			else
				return fvm[index + 1];
		}
		
		else if(line == fvm[index + 1] - 1)
		{
			
			if(fvmcount == index + 2)
				return -1;
			
			else
				return fvm[index + 2];
		}
		
		else
			return line + 1;
	} 

	
	
	public int getPrevVisibleLine(int line)
	{
		int index = fvmget(line);
		
		if(index == -1)
			return -1;
		
		else if(index % 2 == 1)
		{
			
			return fvm[index] - 1;
		}
		
		else if(line == fvm[index])
		{
			
			if(index == 0)
				return -1;
			
			else
				return fvm[index - 1] - 1;
		}
		
		else
			return line - 1;
	} 

	
	public final int getScreenLineCount(int line)
	{
		if(lineMgr.isScreenLineCountValid(line))
			return lineMgr.getScreenLineCount(line);
		else
		{
			int newCount = textArea.chunkCache
				.getLineSubregionCount(line);

			setScreenLineCount(line,newCount);
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

		_notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	
	public int expandFold(int line, boolean fully)
	{
		
		int returnValue = -1;

		int lineCount = buffer.getLineCount();
		int start = 0;
		int end = lineCount - 1;

		int initialFoldLevel = buffer.getFoldLevel(line);

		
		if(line != lineCount - 1
			&& isLineVisible(line)
			&& !isLineVisible(line + 1)
			&& buffer.getFoldLevel(line + 1) > initialFoldLevel)
		{
			

			int index = fvmget(line + 1);
			if(index == -1)
			{
				expandAllFolds();
				return -1;
			}

			start = fvm[index];
			if(index != fvmcount - 1)
				end = fvm[index + 1] - 1;
			else
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
		}
		else
		{
			int index = fvmget(line);
			if(index == -1)
			{
				expandAllFolds();
				return -1;
			}

			start = fvm[index];
			if(index != fvmcount - 1)
				end = fvm[index + 1] - 1;
			else
			{
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

		_notifyScreenLineChanges();
		textArea.foldStructureChanged();

		return returnValue;
	} 

	
	
	public void expandAllFolds()
	{
		showLineRange(0,buffer.getLineCount() - 1);
		_notifyScreenLineChanges();
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

		_notifyScreenLineChanges();
		textArea.foldStructureChanged();
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

		
		
		GUIUtilities.getView(textArea).getStatus().setMessageAndClear(
			jEdit.getProperty("view.status.narrow"));

		_notifyScreenLineChanges();
		textArea.foldStructureChanged();
	} 

	
	boolean softWrap;
	int wrapMargin;
	FirstLine firstLine;
	ScrollLineCount scrollLineCount;

	
	void init()
	{
		if(!initialized)
		{
			initialized = true;
			fvm = new int[2];
			if(buffer.isLoaded())
				bufferChangeHandler.foldHandlerChanged(buffer);
			else
				fvmreset();
			_notifyScreenLineChanges();
		}
		else
		{
			updateWrapSettings();
			_notifyScreenLineChanges();
			textArea.updateScrollBars();
			textArea.recalculateLastPhysicalLine();
		}
	} 

	
	
	void setScreenLineCount(int line, int count)
	{
		int oldCount = lineMgr.getScreenLineCount(line);
		
		
		
		lineMgr.setScreenLineCount(line,count);
		
		
		if(count != oldCount)
		{
			Iterator iter = ((List)bufferMap.get(buffer))
				.iterator();
			while(iter.hasNext())
			{
				((DisplayManager)iter.next())._setScreenLineCount(
					line,oldCount,count);
			}
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

	
	void _notifyScreenLineChanges()
	{
		if(Debug.SCROLL_DEBUG)
			Log.log(Log.DEBUG,this,"_notifyScreenLineChanges()");

		
		
		if(textArea.getDisplayManager() == this)
		{
			try
			{
				if(firstLine.callReset)
					firstLine.reset();
				else if(firstLine.callChanged)
					firstLine.changed();

				if(scrollLineCount.callReset)
					scrollLineCount.reset();
				else if(scrollLineCount.callChanged)
					scrollLineCount.changed();
			}
			finally
			{
				firstLine.callReset = firstLine.callChanged = false;
				scrollLineCount.callReset = scrollLineCount.callChanged = false;
			}
		}
	} 

	

	
	private boolean initialized;
	private boolean inUse;
	private Buffer buffer;
	private LineManager lineMgr;
	private JEditTextArea textArea;
	private BufferChangeHandler bufferChangeHandler;

	
	private int[] fvm;
	private int fvmcount;

	private int lastfvmget = -1;

	
	private DisplayManager(Buffer buffer, JEditTextArea textArea)
	{
		this.buffer = buffer;
		this.lineMgr = buffer._getLineManager();
		this.textArea = textArea;

		scrollLineCount = new ScrollLineCount();
		firstLine = new FirstLine();

		bufferChangeHandler = new BufferChangeHandler();
		
		buffer.addBufferChangeListener(bufferChangeHandler,
			Buffer.HIGH_PRIORITY);
	} 

	
	private void dispose()
	{
		buffer.removeBufferChangeListener(bufferChangeHandler);
	} 

	
	private void fvmreset()
	{
		lastfvmget = -1;
		fvmcount = 2;
		fvm[0] = 0;
		fvm[1] = buffer.getLineCount();
	} 

	
	
	private int fvmget(int line)
	{
		scanCount++;

		if(line < fvm[0])
			return -1;
		if(line >= fvm[fvmcount - 1])
			return fvmcount - 1;

		if(lastfvmget != -1)
		{
			if(line >= fvm[lastfvmget])
			{
				if(lastfvmget == fvmcount - 1
					|| line < fvm[lastfvmget + 1])
				{
					return lastfvmget;
				}
			}
		}

		int start = 0;
		int end = fvmcount - 1;

loop:		for(;;)
		{
			scannedLines++;
			switch(end - start)
			{
			case 0:
				lastfvmget = start;
				break loop;
			case 1:
				int value = fvm[end];
				if(value <= line)
					lastfvmget = end;
				else
					lastfvmget = start;
				break loop;
			default:
				int pivot = (end + start) / 2;
				value = fvm[pivot];
				if(value == line)
				{
					lastfvmget = pivot;
					break loop;
				}
				else if(value < line)
					start = pivot;
				else
					end = pivot - 1;
				break;
			}
		}

		return lastfvmget;
	} 

	
	
	private void fvmput(int start, int end, int[] put)
	{
		if(Debug.FOLD_VIS_DEBUG)
		{
			StringBuffer buf = new StringBuffer("{");
			if(put != null)
			{
				for(int i = 0; i < put.length; i++)
				{
					if(i != 0)
						buf.append(',');
					buf.append(put[i]);
				}
			}
			buf.append("}");
			Log.log(Log.DEBUG,this,"fvmput(" + start + ","
				+ end + "," + buf + ")");
		}
		int putl = (put == null ? 0 : put.length);

		int delta = putl - (end - start);
		if(fvmcount + delta > fvm.length)
		{
			int[] newfvm = new int[fvm.length * 2 + 1];
			System.arraycopy(fvm,0,newfvm,0,fvmcount);
			fvm = newfvm;
		}

		if(delta != 0)
		{
			System.arraycopy(fvm,end,fvm,start + putl,
				fvmcount - end);
		}

		if(putl != 0)
		{
			System.arraycopy(put,0,fvm,start,put.length);
		}

		fvmcount += delta;

		fvmdump();

		if(fvmcount == 0)
			throw new InternalError();
	} 

	
	
	private void fvmput2(int starti, int endi, int start, int end)
	{
		if(Debug.FOLD_VIS_DEBUG)
		{
			Log.log(Log.DEBUG,this,"*fvmput2(" + starti + ","
				+ endi + "," + start + "," + end + ")");
		}
		if(starti != -1 && fvm[starti] == start)
		{
			if(endi <= fvmcount - 2 && fvm[endi + 1]
				== end + 1)
			{
				fvmput(starti,endi + 2,null);
			}
			else
			{
				fvmput(starti,endi + 1,
					new int[] { end + 1 });
			}
		}
		else
		{
			if(endi != fvmcount - 1 && fvm[endi + 1]
				== end + 1)
			{
				fvmput(starti + 1,endi + 2,
					new int[] { start });
			}
			else
			{
				fvmput(starti + 1,endi + 1,
					new int[] { start,
					end + 1 });
			}
		}
	} 

	
	private void fvmdump()
	{
		if(Debug.FOLD_VIS_DEBUG)
		{
			StringBuffer buf = new StringBuffer("{");
			for(int i = 0; i < fvmcount; i++)
			{
				if(i != 0)
					buf.append(',');
				buf.append(fvm[i]);
			}
			buf.append("}");
			Log.log(Log.DEBUG,this,"fvm = " + buf);
		}
	} 

	
	private void showLineRange(int start, int end)
	{
		if(Debug.FOLD_VIS_DEBUG)
		{
			Log.log(Log.DEBUG,this,"showLineRange(" + start
				+ "," + end + ")");
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

		
		int starti = fvmget(start);
		int endi = fvmget(end);

		if(starti % 2 == 0)
		{
			if(endi % 2 == 0)
				fvmput(starti + 1,endi + 1,null);
			else
			{
				if(endi != fvmcount - 1
					&& fvm[endi + 1] == end + 1)
					fvmput(starti + 1,endi + 2,null);
				else
				{
					fvmput(starti + 1,endi,null);
					fvm[starti + 1] = end + 1;
				}
			}
		}
		else
		{
			if(endi % 2 == 0)
			{
				if(starti != -1 && fvm[starti] == start)
					fvmput(starti,endi + 1,null);
				else
				{
					fvmput(starti + 1,endi,null);
					fvm[starti + 1] = start;
				}
			}
			else
				fvmput2(starti,endi,start,end);
		}

		lastfvmget = -1;
	} 

	
	private void hideLineRange(int start, int end)
	{
		if(Debug.FOLD_VIS_DEBUG)
		{
			Log.log(Log.DEBUG,this,"hideLineRange(" + start
				+ "," + end + ")");
		}

		int i = start;
		if(!isLineVisible(i))
			i = getNextVisibleLine(i);
		while(i != -1 && i <= end)
		{
			int screenLines = lineMgr.getScreenLineCount(i);
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

		
		int starti = fvmget(start);
		int endi = fvmget(end);

		if(starti % 2 == 0)
		{
			if(endi % 2 == 0)
				fvmput2(starti,endi,start,end);
			else
			{
				fvmput(starti + 1,endi,null);
				fvm[starti + 1] = start;
			}
		}
		else
		{
			if(endi % 2 == 0)
			{
				fvmput(starti + 1,endi,null);
				fvm[starti + 1] = end + 1;
			}
			else
				fvmput(starti + 1,endi + 1,null);
		}

		lastfvmget = -1;

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
				firstLine.scrollLine -=
					lineMgr.getScreenLineCount(
					firstLine.physicalLine);
			}
			firstLine.callChanged = true;
		}
	} 

	
	private void _setScreenLineCount(int line, int oldCount, int count)
	{
		if(!isLineVisible(line))
			return;

		if(firstLine.physicalLine >= line)
		{
			if(firstLine.physicalLine == line)
				firstLine.callChanged = true;
			else
			{
				firstLine.scrollLine += (count - oldCount);
				firstLine.callChanged = true;
			}
		}

		scrollLineCount.scrollLine += (count - oldCount);
		scrollLineCount.callChanged = true;
	} 

	

	
	static abstract class Anchor
	{
		int physicalLine;
		int scrollLine;
		boolean callChanged;
		boolean callReset;

		abstract void reset();
		abstract void changed();

		public String toString()
		{
			return getClass().getName() + "[" + physicalLine + ","
				+ scrollLine + "]";
		}
	} 

	
	class ScrollLineCount extends Anchor
	{
		
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

			physicalLine = getFirstVisibleLine();
			scrollLine = 0;
			while(physicalLine != -1)
			{
				scrollLine += getScreenLineCount(physicalLine);
				physicalLine = getNextVisibleLine(physicalLine);
			}

			physicalLine = buffer.getLineCount();

			firstLine.ensurePhysicalLineIsVisible();

			textArea.recalculateLastPhysicalLine();
			textArea.updateScrollBars();
		} 
	} 

	
	class FirstLine extends Anchor
	{
		int skew;

		
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

			
			if(Debug.SCROLL_VERIFY)
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

			if(!scrollLineCount.callChanged
				&& !scrollLineCount.callReset)
			{
				textArea.updateScrollBars();
				textArea.recalculateLastPhysicalLine();
			}
			else
			{
				
				
			}
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

			int i = getFirstVisibleLine();

			for(;;)
			{
				if(i >= physicalLine)
					break;

				scrollLine += getScreenLineCount(i);

				int nextLine = getNextVisibleLine(i);
				if(nextLine == -1)
					break;
				else
					i = nextLine;
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

			if(!isLineVisible(physicalLine))
			{
				int lastVisibleLine = getLastVisibleLine();
				if(physicalLine > lastVisibleLine)
					physicalLine = lastVisibleLine;
				else
				{
					int nextPhysicalLine = getNextVisibleLine(physicalLine);
					amount -= (nextPhysicalLine - physicalLine);
					scrollLine += getScreenLineCount(physicalLine);
					physicalLine = nextPhysicalLine;
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

			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"physDown() end: "
					+ physicalLine + ":" + scrollLine);
			}

			callChanged = true;

			
			
			if(screenAmount < 0)
				scrollUp(-screenAmount);
			else if(screenAmount > 0)
				scrollDown(screenAmount);
		} 

		
		
		void physUp(int amount, int screenAmount)
		{
			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"physUp() start: "
					+ physicalLine + ":" + scrollLine);
			}

			skew = 0;

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
					amount -= (physicalLine - prevPhysicalLine);
					physicalLine = prevPhysicalLine;
					scrollLine -= getScreenLineCount(
						prevPhysicalLine);
				}
			}

			if(Debug.SCROLL_DEBUG)
			{
				Log.log(Log.DEBUG,this,"physUp() end: "
					+ physicalLine + ":" + scrollLine);
			}

			callChanged = true;

			
			
			if(screenAmount < 0)
				scrollUp(-screenAmount);
			else if(screenAmount > 0)
				scrollDown(screenAmount);
		} 

		
		
		void scrollDown(int amount)
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"scrollDown()");

			ensurePhysicalLineIsVisible();

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

			callChanged = true;
		} 

		
		
		void scrollUp(int amount)
		{
			if(Debug.SCROLL_DEBUG)
				Log.log(Log.DEBUG,this,"scrollUp()");

			ensurePhysicalLineIsVisible();

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

			callChanged = true;
		} 

		
		private void ensurePhysicalLineIsVisible()
		{
			if(!isLineVisible(physicalLine))
			{
				if(physicalLine > getLastVisibleLine())
				{
					physicalLine = getLastVisibleLine();
					scrollLine = getScrollLineCount() - 1;
				}
				else if(physicalLine < getFirstVisibleLine())
				{
					physicalLine = getFirstVisibleLine();
					scrollLine = 0;
				}
				else
				{
					physicalLine = getNextVisibleLine(physicalLine);
					scrollLine += getScreenLineCount(physicalLine);
				}
			}
		} 
	} 

	
	
	class BufferChangeHandler extends BufferChangeAdapter
	{
		boolean delayedUpdate;
		boolean delayedMultilineUpdate;
		int delayedUpdateStart;
		int delayedUpdateEnd;

		
		public void foldHandlerChanged(Buffer buffer)
		{
			fvmreset();

			firstLine.callReset = true;
			scrollLineCount.callReset = true;

			int collapseFolds = buffer.getIntegerProperty(
				"collapseFolds",0);
			if(collapseFolds != 0)
				expandFolds(collapseFolds);

			_notifyScreenLineChanges();
		} 

		
		public void foldLevelChanged(Buffer buffer, int start, int end)
		{
			if(textArea.getDisplayManager() == DisplayManager.this
				&& end != 0 && buffer.isLoaded())
			{
				textArea.invalidateLineRange(start - 1,
					textArea.getLastPhysicalLine());
			}
		} 

		
		public void contentInserted(Buffer buffer, int startLine,
			int offset, int numLines, int length)
		{
			if(!buffer.isLoaded())
				return;

			int endLine = startLine + numLines;

			if(numLines != 0)
			{
				delayedMultilineUpdate = true;

				
				int index = fvmget(startLine);
				int start = index + 1;
				

				for(int i = start; i < fvmcount; i++)
				{
					fvm[i] += numLines;
				}

				lastfvmget = -1;
				fvmdump();

			}

			if(textArea.getDisplayManager() == DisplayManager.this)
			{
				if(numLines != 0)
				{
					contentInserted(firstLine,startLine,numLines);
					contentInserted(scrollLineCount,startLine,numLines);
				}

				if(delayedUpdateEnd >= startLine)
					delayedUpdateEnd += numLines;
				delayedUpdate(startLine,endLine);

				
				for(int i = 0; i < textArea.selection.size(); i++)
				{
					Selection s = (Selection)textArea
						.selection.elementAt(i);
	
					if(s.contentInserted(buffer,startLine,offset,
						numLines,length))
					{
						delayedUpdate(s.startLine,s.endLine);
					}
				} 

				int caret = textArea.getCaretPosition();
				if(caret >= offset)
				{
					int scrollMode = (caretAutoScroll()
						? JEditTextArea.ELECTRIC_SCROLL
						: JEditTextArea.NO_SCROLL);
					textArea.moveCaretPosition(
						caret + length,scrollMode);
				}
				else
				{
					int scrollMode = (caretAutoScroll()
						? JEditTextArea.NORMAL_SCROLL
						: JEditTextArea.NO_SCROLL);
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

		
		public void preContentRemoved(Buffer buffer, int startLine,
			int offset, int numLines, int length)
		{
			if(!buffer.isLoaded())
				return;

			if(textArea.getDisplayManager() == DisplayManager.this)
			{
				if(numLines != 0)
				{
					preContentRemoved(firstLine,startLine,numLines);
					preContentRemoved(scrollLineCount,startLine,numLines);
				}

				if(delayedUpdateEnd >= startLine)
					delayedUpdateEnd -= numLines;
				delayedUpdate(startLine,startLine);
			}
			else
			{
				firstLine.callReset = true;
				scrollLineCount.callReset = true;
			}

			if(numLines != 0)
			{
				delayedMultilineUpdate = true;

				int endLine = startLine + numLines;

				
				int starti = fvmget(startLine);
				int endi = fvmget(endLine);

				
				if(Math.abs(starti % 2) == Math.abs(endi % 2))
				{
					if(endi - starti == fvmcount)
					{
						
						
						
						fvmreset();
						firstLine.callReset = true;
						scrollLineCount.callReset = true;
					}
					else
					{
						fvmput(starti + 1,endi + 1,null);
						starti++;
					}
				}
				
				else if(starti != -1 && fvm[starti] == startLine)
				{
					
					fvmput(starti,endi + 1,null);
					
					
				}
				
				else
				{
					fvmput(starti + 1,endi,null);
					fvm[starti + 1] = startLine;
					starti += 2;
				}

				
				for(int i = starti; i < fvmcount; i++)
					fvm[i] -= numLines;

				if(firstLine.physicalLine
					> getLastVisibleLine()
					|| firstLine.physicalLine
					< getFirstVisibleLine())
				{
					
					
					
				}
				
				
				
				
				else if(!isLineVisible(
					firstLine.physicalLine))
				{
					firstLine.physicalLine =
						getNextVisibleLine(
						firstLine.physicalLine);
				}

				lastfvmget = -1;
				fvmdump();
			}
		} 

		
		public void contentRemoved(Buffer buffer, int startLine,
			int start, int numLines, int length)
		{
			if(!buffer.isLoaded())
				return;

			if(textArea.getDisplayManager() == DisplayManager.this)
			{
				int endLine = startLine + numLines;

				
				for(int i = 0; i < textArea.selection.size(); i++)
				{
					Selection s = (Selection)textArea
						.selection.elementAt(i);
	
					if(s.contentRemoved(buffer,startLine,
						start,numLines,length))
					{
						delayedUpdate(s.startLine,s.endLine);
						if(s.start == s.end)
						{
							textArea.selection.removeElementAt(i);
							i--;
						}
					}
				} 

				int caret = textArea.getCaretPosition();

				if(caret >= start + length)
				{
					int scrollMode = (caretAutoScroll()
						? JEditTextArea.ELECTRIC_SCROLL
						: JEditTextArea.NO_SCROLL);
					textArea.moveCaretPosition(
						caret - length,
						scrollMode);
				}
				else if(caret >= start)
				{
					int scrollMode = (caretAutoScroll()
						? JEditTextArea.ELECTRIC_SCROLL
						: JEditTextArea.NO_SCROLL);
					textArea.moveCaretPosition(
						start,scrollMode);
				}
				else
				{
					int scrollMode = (caretAutoScroll()
						? JEditTextArea.NORMAL_SCROLL
						: JEditTextArea.NO_SCROLL);
					textArea.moveCaretPosition(caret,scrollMode);
				}
			}
		}
		

		
		public void transactionComplete(Buffer buffer)
		{
			if(textArea.getDisplayManager() == DisplayManager.this)
			{
				if(delayedUpdate)
				{
					
					
					
					
					_notifyScreenLineChanges();

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

					int _firstLine = textArea.getFirstPhysicalLine();
					int _lastLine = textArea.getLastPhysicalLine();

					int line = delayedUpdateStart;
					if(!isLineVisible(line))
						line = getNextVisibleLine(line);
					while(line != -1 && line <= delayedUpdateEnd)
					{
						if(line < _firstLine
							|| line > _lastLine)
						{
							getScreenLineCount(line);
						}
						line = getNextVisibleLine(line);
					}

					
					int visibleLines = textArea
						.getVisibleLines();
					if(visibleLines != 0)
					{
						textArea.chunkCache.getLineInfo(
							visibleLines - 1);
					}
				}

				textArea._finishCaretUpdate();
			}

			
			if(Debug.SCROLL_VERIFY)
			{
				int scrollLineCount = 0;
				for(int i = 0; i < textArea.getLineCount(); i++)
				{
					if(isLineVisible(i))
					{
						scrollLineCount +=
							getScreenLineCount(i);
					}
				}
				if(scrollLineCount != getScrollLineCount())
				{
					throw new InternalError(scrollLineCount
						+ " != "
						+ getScrollLineCount());
				}
			} 

			delayedUpdate = false;
		} 

		
		private void contentInserted(Anchor anchor, int startLine,
			int numLines)
		{
			if(anchor.physicalLine >= startLine)
			{
				if(anchor.physicalLine != startLine)
					anchor.physicalLine += numLines;
				anchor.callChanged = true;
			}
		} 

		
		private void preContentRemoved(Anchor anchor, int startLine,
			int numLines)
		{
			if(anchor.physicalLine >= startLine)
			{
				if(anchor.physicalLine == startLine)
					anchor.callChanged = true;
				else
				{
					int end = Math.min(startLine + numLines,
						anchor.physicalLine);
					for(int i = startLine; i < end; i++)
					{
						
						if(isLineVisible(i))
						{
							anchor.scrollLine -=
								lineMgr
								.getScreenLineCount(i);
						}
					}
					anchor.physicalLine -= (end - startLine);
					anchor.callChanged = true;
				}
			}
		} 

		
		private void delayedUpdate(int startLine, int endLine)
		{
			textArea.chunkCache.invalidateChunksFromPhys(startLine);
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

		
		
		private boolean caretAutoScroll()
		{
			View view = textArea.getView();
			return view == jEdit.getActiveView()
				&& view.getTextArea() == textArea;
		} 
	} 
}
