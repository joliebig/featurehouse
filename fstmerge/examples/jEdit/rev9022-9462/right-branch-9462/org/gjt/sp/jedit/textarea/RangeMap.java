

package org.gjt.sp.jedit.textarea;

import org.gjt.sp.jedit.Debug;
import org.gjt.sp.util.Log;


class RangeMap
{
	
	RangeMap()
	{
		fvm = new int[2];
		lastfvmget = -1;
	} 

	
	RangeMap(RangeMap copy)
	{
		this.fvm = (int[])copy.fvm.clone();
		this.fvmcount = copy.fvmcount;
	} 

	
	void reset(int lines)
	{
		lastfvmget = -1;
		fvmcount = 2;
		fvm[0] = 0;
		fvm[1] = lines;
	} 

	
	int first()
	{
		return fvm[0];
	} 

	
	int last()
	{
		return fvm[fvmcount - 1] - 1;
	} 

	
	int lookup(int index)
	{
		return fvm[index];
	} 

	
	
	int search(int line)
	{
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

	
	
	void put(int start, int end, int[] put)
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

		dump();

		if(fvmcount == 0)
			throw new InternalError();
	} 

	
	
	void put2(int starti, int endi, int start, int end)
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
				put(starti,endi + 2,null);
			}
			else
			{
				put(starti,endi + 1,
					new int[] { end + 1 });
			}
		}
		else
		{
			if(endi != fvmcount - 1 && fvm[endi + 1]
				== end + 1)
			{
				put(starti + 1,endi + 2,
					new int[] { start });
			}
			else
			{
				put(starti + 1,endi + 1,
					new int[] { start,
					end + 1 });
			}
		}
	} 

	
	int next(int line)
	{
		int index = search(line);
		
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

	
	int prev(int line)
	{
		int index = search(line);
		
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

	
	void show(int start, int end)
	{
		int starti = search(start);
		int endi = search(end);

		if(starti % 2 == 0)
		{
			if(endi % 2 == 0)
				put(starti + 1,endi + 1,null);
			else
			{
				if(endi != fvmcount - 1
					&& fvm[endi + 1] == end + 1)
					put(starti + 1,endi + 2,null);
				else
				{
					put(starti + 1,endi,null);
					fvm[starti + 1] = end + 1;
				}
			}
		}
		else
		{
			if(endi % 2 == 0)
			{
				if(starti != -1 && fvm[starti] == start)
					put(starti,endi + 1,null);
				else
				{
					put(starti + 1,endi,null);
					fvm[starti + 1] = start;
				}
			}
			else
				put2(starti,endi,start,end);
		}

		lastfvmget = -1;
	} 
	
	
	void hide(int start, int end)
	{
		int starti = search(start);
		int endi = search(end);

		if(starti % 2 == 0)
		{
			if(endi % 2 == 0)
				put2(starti,endi,start,end);
			else
			{
				if(start == fvm[0])
					put(starti,endi + 1,null);
				else
				{
					put(starti + 1,endi,null);
					fvm[starti + 1] = start;
				}
			}
		}
		else
		{
			if(endi % 2 == 0)
			{
				if(end + 1 == fvm[fvmcount - 1])
					put(starti + 1,endi + 2,null);
				else
				{
					put(starti + 1,endi,null);
					fvm[starti + 1] = end + 1;
				}
			}
			else
				put(starti + 1,endi + 1,null);
		}

		lastfvmget = -1;
	} 

	
	int count()
	{
		return fvmcount;
	} 

	
	void dump()
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

	
	void contentInserted(int startLine, int numLines)
	{
		if(numLines != 0)
		{
			int index = search(startLine);
			int start = index + 1;

			for(int i = start; i < fvmcount; i++)
				fvm[i] += numLines;

			lastfvmget = -1;
			dump();
		}
	} 

	
	
	boolean preContentRemoved(int startLine, int numLines)
	{
		boolean returnValue = false;

		int endLine = startLine + numLines;

		
		int starti = search(startLine);
		int endi = search(endLine);

		
		if(Math.abs(starti % 2) == Math.abs(endi % 2))
		{
			if(endi - starti == fvmcount)
			{
				
				
				
				returnValue = true;
				starti = 1;
			}
			else
			{
				put(starti + 1,endi + 1,null);
				starti++;
			}
		}
		
		else if(starti != -1 && fvm[starti] == startLine)
		{
			if(endi - starti == fvmcount - 1)
			{
				
				
				
				returnValue = true;
				starti = 1;
			}
			else
				put(starti,endi + 1,null);
		}
		
		else
		{
			put(starti + 1,endi,null);
			fvm[starti + 1] = startLine;
			starti += 2;
		}

		
		for(int i = starti; i < fvmcount; i++)
			fvm[i] -= numLines;

		lastfvmget = -1;
		dump();

		return returnValue;
	} 

	
	private int[] fvm;
	private int fvmcount;
	private int lastfvmget;
	
}
