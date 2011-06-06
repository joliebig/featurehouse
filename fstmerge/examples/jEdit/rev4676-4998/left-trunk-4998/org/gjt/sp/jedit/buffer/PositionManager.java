

package org.gjt.sp.jedit.buffer;


import javax.swing.text.Position;
import java.util.*;
import org.gjt.sp.util.Log;



public class PositionManager
{
	
	public synchronized Position createPosition(int offset)
	{
		PosBottomHalf bh = new PosBottomHalf(offset);
		PosBottomHalf existing = (PosBottomHalf)positions.get(bh);
		if(existing == null)
		{
			positions.put(bh,bh);
			existing = bh;
		}

		return new PosTopHalf(existing);
	} 

	
	public synchronized void contentInserted(int offset, int length)
	{
		if(positions.size() == 0)
			return;

		
		Iterator iter = positions.tailMap(new PosBottomHalf(offset))
			.keySet().iterator();

		iteration = true;
		while(iter.hasNext())
		{
			PosBottomHalf bh = (PosBottomHalf)iter.next();
			bh.offset += length;
		}
		iteration = false;
	} 

	
	public synchronized void contentRemoved(int offset, int length)
	{
		if(positions.size() == 0)
			return;

		
		Iterator iter = positions.tailMap(new PosBottomHalf(offset))
			.keySet().iterator();

		iteration = true;
		while(iter.hasNext())
		{
			PosBottomHalf bh = (PosBottomHalf)iter.next();
			if(bh.offset <= offset + length)
				bh.offset = offset;
			else
				bh.offset -= length;
		}
		iteration = false;

	} 

	boolean iteration;

	
	private SortedMap positions = new TreeMap();
	

	

	
	class PosTopHalf implements Position
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

		
		protected void finalize()
		{
			synchronized(PositionManager.this)
			{
				bh.unref();
			}
		} 
	} 

	
	class PosBottomHalf implements Comparable
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
				positions.remove(this);
		} 

		
		public boolean equals(Object o)
		{
			if(!(o instanceof PosBottomHalf))
				return false;

			return ((PosBottomHalf)o).offset == offset;
		} 

		
		public int compareTo(Object o)
		{
			if(iteration)
				Log.log(Log.ERROR,this,"Consistency failure");
			return offset - ((PosBottomHalf)o).offset;
		} 
	} 

	
}
