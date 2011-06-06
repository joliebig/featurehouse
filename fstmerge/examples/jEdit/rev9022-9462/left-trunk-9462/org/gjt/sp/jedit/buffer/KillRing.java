

package org.gjt.sp.jedit.buffer;

import javax.swing.event.ListDataListener;
import java.util.*;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.jedit.gui.MutableListModel;
import org.gjt.sp.util.XMLUtilities;


public class KillRing implements MutableListModel
{
	
	public static KillRing getInstance()
	{
		return killRing;
	} 

	
	public static void setInstance(KillRing killRing)
	{
		KillRing.killRing = killRing;
	} 

	
	public void propertiesChanged(int historySize)
	{
		int newSize = Math.max(1, historySize);
		if(ring == null)
			ring = new UndoManager.Remove[newSize];
		else if(newSize != ring.length)
		{
			UndoManager.Remove[] newRing = new UndoManager.Remove[
				newSize];
			int newCount = Math.min(getSize(),newSize);
			for(int i = 0; i < newCount; i++)
			{
				newRing[i] = (UndoManager.Remove)getElementAt(i);
			}
			ring = newRing;
			count = newCount;
			wrap = false;
		}

		if(count == ring.length)
		{
			count = 0;
			wrap = true;
		}
	} 

	public void load() {}

	public void save() {}

	
	public void addListDataListener(ListDataListener listener) {}

	public void removeListDataListener(ListDataListener listener) {}

	
	public Object getElementAt(int index)
	{
		return ring[virtualToPhysicalIndex(index)];
	} 

	
	public int getSize()
	{
		if(wrap)
			return ring.length;
		else
			return count;
	} 

	
	public boolean removeElement(Object value)
	{
		for(int i = 0; i < getSize(); i++)
		{
			if(ring[i].equals(value))
			{
				remove(i);
				return true;
			}
		}
		return false;
	} 

	
	public void insertElementAt(Object value, int index)
	{
		
		remove(index);
		add((UndoManager.Remove)value);
	} 

	

	

	
	void changed(UndoManager.Remove rem)
	{
		if(rem.inKillRing)
		{
			
			int length = (wrap ? ring.length : count);
			int kill = -1;

			for(int i = 0; i < length; i++)
			{
				if(ring[i] != rem
					&& ring[i].hashcode == rem.hashcode
					&& ring[i].str.equals(rem.str))
				{
					
					
					kill = i;
					break;
				}
			}

			if(kill != -1)
				remove(kill);
		}
		else
			add(rem);
	} 

	
	void add(UndoManager.Remove rem)
	{
		
		int length = (wrap ? ring.length : count);
		for(int i = 0; i < length; i++)
		{
			if(ring[i].hashcode == rem.hashcode)
			{
				
				if(ring[i].str.equals(rem.str))
				{
					
					
					return;
				}
			}
		}

		
		boolean allWhitespace = true;
		for(int i = 0; i < rem.str.length(); i++)
		{
			if(!Character.isWhitespace(rem.str.charAt(i)))
			{
				allWhitespace = false;
				break;
			}
		}

		if(allWhitespace)
			return;

		rem.inKillRing = true;

		if(ring[count] != null)
			ring[count].inKillRing = false;

		ring[count] = rem;
		if(++count >= ring.length)
		{
			wrap = true;
			count = 0;
		}
	} 

	
	void remove(int i)
	{
		if(wrap)
		{
			UndoManager.Remove[] newRing = new UndoManager.Remove[
				ring.length];
			int newCount = 0;
			for(int j = 0; j < ring.length; j++)
			{
				int index = virtualToPhysicalIndex(j);

				if(i == index)
				{
					ring[index].inKillRing = false;
					continue;
				}

				newRing[newCount++] = ring[index];
			}
			ring = newRing;
			count = newCount;
			wrap = false;
		}
		else
		{
			System.arraycopy(ring,i + 1,ring,i,count - i - 1);
			count--;
		}
	} 

	

	
	protected UndoManager.Remove[] ring;
	protected int count;
	protected boolean wrap;
	protected long killRingModTime;
	private static KillRing killRing = new KillRing();

	
	
	private int virtualToPhysicalIndex(int index)
	{
		if(wrap)
		{
			if(index < count)
				return count - index - 1;
			else
				return count + ring.length - index - 1;
		}
		else
			return count - index - 1;
	} 

	

	
	public static class KillRingHandler extends DefaultHandler
	{
		public List<UndoManager.Remove> list = new LinkedList<UndoManager.Remove>();

		public InputSource resolveEntity(String publicId, String systemId)
		{
			return XMLUtilities.findEntity(systemId, "killring.dtd", getClass());
		}

		public void startElement(String uri, String localName,
					 String qName, Attributes attrs)
		{
			inEntry = qName.equals("ENTRY");
		}

		public void endElement(String uri, String localName, String name)
		{
			if(name.equals("ENTRY"))
			{
				list.add(new UndoManager.Remove(null,0,0,charData.toString()));
				inEntry = false;
				charData.setLength(0);
			}
		}

		public void characters(char[] ch, int start, int length)
		{
			if (inEntry)
				charData.append(ch, start, length);
		}

		private StringBuffer charData = new StringBuffer();
		private boolean inEntry;
	} 
}
