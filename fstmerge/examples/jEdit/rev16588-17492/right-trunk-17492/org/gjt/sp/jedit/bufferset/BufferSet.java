
package org.gjt.sp.jedit.bufferset;


import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.StandardUtilities;

import javax.swing.event.EventListenerList;
import java.util.List;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;



public class BufferSet
{
	
	public enum Scope
	{
		editpane, view, global;

		public static Scope fromString(String s)
		{
			Scope[] scopes = values();
			for (Scope scope: scopes)
			{
				if (scope.toString().equals(s))
					return scope;
			}

			return global;
		}
	} 

	
	public BufferSet(BufferSet source)
	{
		if (source == null)
			buffers = Collections.synchronizedList(new ArrayList<Buffer>());
		else
			buffers = Collections.synchronizedList(new ArrayList<Buffer>(source.buffers));
		listeners = new EventListenerList();

		if (jEdit.getBooleanProperty("sortBuffers"))
		{
			if (jEdit.getBooleanProperty("sortByName"))
				sorter = nameSorter;
			else
				sorter = pathSorter;
		}
	}

	
	
	public void addBuffer(Buffer buffer)
	{
		addBufferAt(buffer,  -1);
	} 

	
	
	public void addBufferAt(Buffer buffer, int position)
	{
		Log.log(Log.DEBUG, this, hashCode() + " addBufferAt("+buffer+','+position+')');

		Buffer untitledBuffer = null;
		synchronized (buffers)
		{
			if (buffers.size() == 1)
			{
				Buffer buf = buffers.get(0);
				if (buf.isUntitled() && !buf.isDirty())
				{
					untitledBuffer = buf;
				}
			}

			if (sorter != null)
			{
				if (buffers.contains(buffer))
					return;
				buffers.add(buffer);
				Collections.sort(buffers, sorter);
				position = buffers.indexOf(buffer);
			}
			else
			{
				int oldPos = buffers.indexOf(buffer);
				if (oldPos != -1)
				{
					if (position == -1)
					{
						return;
					}
					moveBuffer(oldPos, position);
					return;
				}
				int size = buffers.size();
				if (position == -1 || position > size)
				{
					position = size;
				}
				buffers.add(position, buffer);
			}
		}
		BufferSetListener[] listeners = this.listeners.getListeners(BufferSetListener.class);
		Log.log(Log.DEBUG, this, hashCode() + ": Buffer added " + buffer + " at " + position);
		for (BufferSetListener listener : listeners)
		{
			listener.bufferAdded(buffer, position);
		}

		
		if (untitledBuffer != null)
		{
			jEdit.getBufferSetManager().removeBuffer(this, untitledBuffer);
		}
	} 

	
	
	public Buffer getBuffer(int index)
	{
		return buffers.get(index);
	} 

	
	public Buffer getPreviousBuffer(int index)
	{
		if (buffers.isEmpty())
			return null;
		if (buffers.size() < 2)
			return buffers.get(0);
		if (index <= 0)
			return buffers.get(buffers.size() - 1);
		return buffers.get(index - 1);
	} 

	
	public Buffer getNextBuffer(int index)
	{
		if (buffers.isEmpty())
			return null;
		if (buffers.size() < 2)
			return buffers.get(buffers.size()-1);
		if (index >= buffers.size() - 1)
			return buffers.get(0);
		return buffers.get(index + 1);
	} 

	
	public int indexOf(Buffer buffer)
	{
		return buffers.indexOf(buffer);
	} 

	
	public int size()
	{
		return buffers.size();
	} 

	
	public void getAllBuffers(BufferSetListener listener)
	{
		synchronized (buffers)
		{
			for (int i = 0;i<buffers.size();i++)
			{
				Buffer buffer = buffers.get(i);
				Log.log(Log.DEBUG, this, hashCode() + ": Buffer added " + buffer + " at " + i);
				listener.bufferAdded(buffer, i);
			}
		}
	}

	
	public Buffer[] getAllBuffers()
	{
		Buffer[] buffers = new Buffer[this.buffers.size()];
		return this.buffers.toArray(buffers);
	} 

	
	
	public void addBufferSetListener(BufferSetListener listener)
	{
		Log.log(Log.DEBUG, this, hashCode() + ": addBufferSetListener " + listener);
		listeners.add(BufferSetListener.class, listener);
	} 

	
	
	public void removeBufferSetListener(BufferSetListener listener)
	{
		Log.log(Log.DEBUG, this, hashCode() + ": removeBufferSetListener " + listener);
		listeners.remove(BufferSetListener.class, listener);
	} 

	
	@Override
	public String toString()
	{
		return "BufferSet[nbBuffers="+size()+']';
	} 

	
	
	public void sort()
	{
		if (sorter == null)
			return;
		
		Collections.sort(buffers, sorter);

		
		BufferSetListener[] listeners = this.listeners.getListeners(BufferSetListener.class);
		for (BufferSetListener listener : listeners)
		{
			listener.bufferSetSorted();
		}
	} 

	

	
	
	void propertiesChanged()
	{
		if (jEdit.getBooleanProperty("sortBuffers"))
		{
			
			if (jEdit.getBooleanProperty("sortByName"))
				sorter = nameSorter;
			else
				sorter = pathSorter;

			sort();
		}
		else
		{
			
			sorter = null;
		}
	} 

	
	void moveBuffer(int oldPosition, int newPosition)
	{
		if (sorter != null)
		{
			
			return;
		}
		Buffer buffer;
		synchronized (buffers)
		{
			buffer = buffers.remove(oldPosition);
			int size = buffers.size();
			if (newPosition == -1 || newPosition > size)
			{
				newPosition = size;
			}
			buffers.add(newPosition, buffer);
		}
		BufferSetListener[] listeners = this.listeners.getListeners(BufferSetListener.class);
		Log.log(Log.DEBUG, this, hashCode() + ": Buffer moved " + buffer + " from " + oldPosition + " to " + newPosition);
		for (BufferSetListener listener : listeners)
		{
			listener.bufferMoved(buffer, oldPosition, newPosition);
		}
	} 

	
	void removeBuffer(Buffer buffer)
	{
		int index;
		synchronized (buffers)
		{
			index = buffers.indexOf(buffer);
			if (index == -1)
				return;

			buffers.remove(index);
		}
		BufferSetListener[] listeners = this.listeners.getListeners(BufferSetListener.class);
		Log.log(Log.DEBUG, this, hashCode() + ": Buffer removed " + buffer);
		for (BufferSetListener listener : listeners)
		{
			listener.bufferRemoved(buffer, index);
		}
	} 

	

	
	private final List<Buffer> buffers;
	private final EventListenerList listeners;
	private static final Comparator<Buffer> nameSorter = new NameSorter();
	private static final Comparator<Buffer> pathSorter = new PathSorter();
	private Comparator<Buffer> sorter;
	


	
	private static class NameSorter implements Comparator<Buffer>
	{
		public int compare(Buffer o1, Buffer o2)
		{

			int ret = StandardUtilities.compareStrings(o1.getName(), o2.getName(), true);
			if (ret == 0)
			{
				ret = StandardUtilities.compareStrings(o1.getPath(), o2.getPath(), true);
			}
			return ret;
		}
	} 

	
	private static class PathSorter implements Comparator<Buffer>
	{
		public int compare(Buffer o1, Buffer o2)
		{
			return StandardUtilities.compareStrings(o1.getPath(), o2.getPath(), true);
		}
	} 
}
