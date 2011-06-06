

package org.gjt.sp.jedit.buffer;


import javax.swing.text.Position;
import java.util.*;
import org.gjt.sp.util.Log;



public class PositionManager
{
	
	public PositionManager(JEditBuffer buffer)
	{
		this.buffer = buffer;
	} 
	
	
	public synchronized Position createPosition(int offset)
	{
		PosBottomHalf bh = new PosBottomHalf(offset);
		PosBottomHalf existing = positions.get(bh);
		if(existing == null)
		{
			positions.put(bh,bh);
			existing = bh;
		}

		return new PosTopHalf(existing);
	} 

	
	public synchronized void contentInserted(int offset, int length)
	{
		if(positions.isEmpty())
			return;

		
		Iterator iter = positions.tailMap(new PosBottomHalf(offset))
			.keySet().iterator();

		iteration = true;
		while(iter.hasNext())
		{
			((PosBottomHalf)iter.next())
				.contentInserted(offset,length);
		}
		iteration = false;
	} 

	
	public synchronized void contentRemoved(int offset, int length)
	{
		if(positions.isEmpty())
			return;

		
		Iterator iter = positions.tailMap(new PosBottomHalf(offset))
			.keySet().iterator();

		iteration = true;
		while(iter.hasNext())
		{
			((PosBottomHalf)iter.next())
				.contentRemoved(offset,length);
		}
		iteration = false;

	} 

	boolean iteration;

	
	private JEditBuffer buffer;
	private SortedMap<PosBottomHalf, PosBottomHalf> positions = new TreeMap<PosBottomHalf, PosBottomHalf>();
	

	

	
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

	
	class PosBottomHalf implements Comparable<PosBottomHalf>
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

		
		void contentInserted(int offset, int length)
		{
			if(offset > this.offset)
				throw new ArrayIndexOutOfBoundsException();
			this.offset += length;
			checkInvariants();
		} 

		
		void contentRemoved(int offset, int length)
		{
			if(offset > this.offset)
				throw new ArrayIndexOutOfBoundsException();
			if(this.offset <= offset + length)
				this.offset = offset;
			else
				this.offset -= length;
			checkInvariants();
		} 

		
		public boolean equals(Object o)
		{
			if(!(o instanceof PosBottomHalf))
				return false;

			return ((PosBottomHalf)o).offset == offset;
		} 

		
		public int compareTo(PosBottomHalf posBottomHalf)
		{
			if(iteration)
				Log.log(Log.ERROR,this,"Consistency failure");
			return offset - posBottomHalf.offset;
		} 
		
		
		private void checkInvariants()
		{
			if(offset < 0 || offset > buffer.getLength())
				throw new ArrayIndexOutOfBoundsException();
		} 
	} 

	
}
