

package org.gjt.sp.jedit;


import java.io.IOException;
import java.util.*;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import org.xml.sax.InputSource;
import org.xml.sax.helpers.DefaultHandler;

import org.gjt.sp.jedit.msg.DynamicMenuChanged;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;
import org.gjt.sp.util.XMLUtilities;
import org.gjt.sp.util.IOUtilities;



public class BufferHistory
{
	
	
	public static class Entry
	{
		public String path;
		public int caret;
		public String selection;
		public String encoding;
		public String mode;

		public Selection[] getSelection()
		{
			return stringToSelection(selection);
		}

		public Entry(String path, int caret, String selection, String encoding, String mode)
		{
			this.path = path;
			this.caret = caret;
			this.selection = selection;
			this.encoding = encoding;
			this.mode = mode;
		}

		public String toString()
		{
			return path + ": " + caret;
		}
	} 

	
	public static Entry getEntry(String path)
	{
		historyLock.readLock().lock();
		try
		{
			for (Entry entry : history)
			{
				if(MiscUtilities.pathsEqual(entry.path,path))
					return entry;
			}
		}
		finally
		{
			historyLock.readLock().unlock();
		}

		return null;
	} 

	
	public static void setEntry(String path, int caret, Selection[] selection,
		String encoding, String mode)
	{
		Entry entry = new Entry(path,caret,
			selectionToString(selection), encoding, mode);
		historyLock.writeLock().lock();
		try
		{
			removeEntry(path);
			addEntry(entry);
		}
		finally
		{
			historyLock.writeLock().unlock();
		}
		notifyChange();
	} 

	
	
	public static void clear()
	{
		historyLock.writeLock().lock();
		try
		{
			history.clear();
		}
		finally
		{
			historyLock.writeLock().unlock();
		}
		notifyChange();
	} 

	
	
	public static List<Entry> getHistory()
	{
		
		
		
		

		historyLock.readLock().lock();
		try
		{
			return (List<Entry>)history.clone();
		}
		finally
		{
			historyLock.readLock().unlock();
		}
	} 

	
	public static void load()
	{
		if(recentXML == null)
			return;

		if(!recentXML.fileExists())
			return;

		Log.log(Log.MESSAGE,BufferHistory.class,"Loading " + recentXML);

		RecentHandler handler = new RecentHandler();
		try
		{
			recentXML.load(handler);
		}
		catch(IOException e)
		{
			Log.log(Log.ERROR,BufferHistory.class,e);
		}
		trimToLimit(handler.result);
		history = handler.result;
	} 

	
	public static void save()
	{
		if(recentXML == null)
			return;

		if(recentXML.hasChangedOnDisk())
		{
			Log.log(Log.WARNING,BufferHistory.class,recentXML
				+ " changed on disk; will not save recent"
				+ " files");
			return;
		}

		Log.log(Log.MESSAGE,BufferHistory.class,"Saving " + recentXML);

		String lineSep = System.getProperty("line.separator");

		SettingsXML.Saver out = null;

		try
		{
			out = recentXML.openSaver();
			out.writeXMLDeclaration();

			out.write("<!DOCTYPE RECENT SYSTEM \"recent.dtd\">");
			out.write(lineSep);
			out.write("<RECENT>");
			out.write(lineSep);

			
			
			List<Entry> snapshot = getHistory();

			for (Entry entry : snapshot)
			{
				out.write("<ENTRY>");
				out.write(lineSep);

				out.write("<PATH>");
				out.write(XMLUtilities.charsToEntities(entry.path,false));
				out.write("</PATH>");
				out.write(lineSep);

				out.write("<CARET>");
				out.write(String.valueOf(entry.caret));
				out.write("</CARET>");
				out.write(lineSep);

				if(entry.selection != null
					&& entry.selection.length() > 0)
				{
					out.write("<SELECTION>");
					out.write(entry.selection);
					out.write("</SELECTION>");
					out.write(lineSep);
				}

				if(entry.encoding != null)
				{
					out.write("<ENCODING>");
					out.write(entry.encoding);
					out.write("</ENCODING>");
					out.write(lineSep);
				}

				if (entry.mode != null)
				{
					out.write("<MODE>");
					out.write(entry.mode);
					out.write("</MODE>");
					out.write(lineSep);
				}

				out.write("</ENTRY>");
				out.write(lineSep);
			}

			out.write("</RECENT>");
			out.write(lineSep);

			out.finish();
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,BufferHistory.class,e);
		}
		finally
		{
			IOUtilities.closeQuietly(out);
		}
	} 

	
	private static LinkedList<Entry> history;
	private static ReentrantReadWriteLock historyLock;
	private static SettingsXML recentXML;

	
	static
	{
		history = new LinkedList<Entry>();
		historyLock = new ReentrantReadWriteLock();
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory != null)
		{
			recentXML = new SettingsXML(settingsDirectory, "recent");
		}
	} 

	
	private static void addEntry(Entry entry)
	{
		historyLock.writeLock().lock();
		try
		{
			history.addFirst(entry);
			trimToLimit(history);
		}
		finally
		{
			historyLock.writeLock().unlock();
		}
	} 

	
	private static void removeEntry(String path)
	{
		historyLock.writeLock().lock();
		try
		{
			Iterator<Entry> iter = history.iterator();
			while(iter.hasNext())
			{
				Entry entry = iter.next();
				if(MiscUtilities.pathsEqual(path,entry.path))
				{
					iter.remove();
					return;
				}
			}
		}
		finally
		{
			historyLock.writeLock().unlock();
		}
	} 

	
	private static String selectionToString(Selection[] s)
	{
		if(s == null)
			return null;

		StringBuilder buf = new StringBuilder();

		for(int i = 0; i < s.length; i++)
		{
			if(i != 0)
				buf.append(' ');

			Selection sel = s[i];
			if(sel instanceof Selection.Range)
				buf.append("range ");
			else 
				buf.append("rect ");
			buf.append(sel.getStart());
			buf.append(' ');
			buf.append(sel.getEnd());
		}

		return buf.toString();
	} 

	
	private static Selection[] stringToSelection(String s)
	{
		if(s == null)
			return null;

		List<Selection> selection = new ArrayList<Selection>();
		StringTokenizer st = new StringTokenizer(s);

		while(st.hasMoreTokens())
		{
			String type = st.nextToken();
			int start = Integer.parseInt(st.nextToken());
			int end = Integer.parseInt(st.nextToken());
			if(end < start)
			{
				
				
				
				continue;
			}

			Selection sel;
			if(type.equals("range"))
				sel = new Selection.Range(start,end);
			else 
				sel = new Selection.Rect(start,end);

			selection.add(sel);
		}

		Selection[] returnValue = new Selection[selection.size()];
		returnValue = selection.toArray(returnValue);
		return returnValue;
	} 

	
	private static void trimToLimit(LinkedList<Entry> list)
	{
		int max = jEdit.getIntegerProperty("recentFiles",50);
		while(list.size() > max)
			list.removeLast();
	} 

	
	private static void notifyChange()
	{
		EditBus.send(new DynamicMenuChanged("recent-files"));
	} 

	
	private static class RecentHandler extends DefaultHandler
	{
		public LinkedList<Entry> result = new LinkedList<Entry>();

		public InputSource resolveEntity(String publicId, String systemId)
		{
			return XMLUtilities.findEntity(systemId, "recent.dtd", getClass());
		}

		public void endElement(String uri, String localName, String name)
		{
			if(name.equals("ENTRY"))
			{
				result.addLast(new Entry(
					path,caret,selection,
					encoding,
					mode));
				path = null;
				caret = 0;
				selection = null;
				encoding = null;
				mode = null;
			}
			else if(name.equals("PATH"))
				path = charData.toString();
			else if(name.equals("CARET"))
				caret = Integer.parseInt(charData.toString());
			else if(name.equals("SELECTION"))
				selection = charData.toString();
			else if(name.equals("ENCODING"))
				encoding = charData.toString();
			else if(name.equals("MODE"))
				mode = charData.toString();
			charData.setLength(0);
		}

		public void characters(char[] ch, int start, int length)
		{
			charData.append(ch,start,length);
		}

		

		
		private String path;
		private int caret;
		private String selection;
		private String encoding;
		private String mode;
		private StringBuilder charData = new StringBuilder();
	} 

	
}
