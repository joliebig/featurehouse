

package org.gjt.sp.jedit;


import com.microstar.xml.*;
import java.io.*;
import java.util.*;
import org.gjt.sp.jedit.msg.DynamicMenuChanged;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class BufferHistory
{
	
	public static Entry getEntry(String path)
	{
		Iterator iter = history.iterator();
		while(iter.hasNext())
		{
			Entry entry = (Entry)iter.next();
			if(pathsCaseInsensitive)
			{
				if(entry.path.equalsIgnoreCase(path))
					return entry;
			}
			else
			{
				if(entry.path.equals(path))
					return entry;
			}
		}

		return null;
	} 

	
	public static void setEntry(String path, int caret, Selection[] selection,
		String encoding)
	{
		removeEntry(path);
		addEntry(new Entry(path,caret,selectionToString(selection),
			encoding));
		EditBus.send(new DynamicMenuChanged("recent-files"));
	} 

	
	
	public static List getHistory()
	{
		return history;
	} 

	
	
	public static Vector getBufferHistory()
	{
		Vector retVal = new Vector(history.size());
		Iterator iter = history.iterator();
		while(iter.hasNext())
			retVal.add(iter.next());
		return retVal;
	} 

	
	public static void load()
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return;

		File recent = new File(MiscUtilities.constructPath(
			settingsDirectory,"recent.xml"));
		if(!recent.exists())
			return;

		recentModTime = recent.lastModified();

		Log.log(Log.MESSAGE,BufferHistory.class,"Loading recent.xml");

		RecentHandler handler = new RecentHandler();
		XmlParser parser = new XmlParser();
		Reader in = null;
		parser.setHandler(handler);
		try
		{
			in = new BufferedReader(new FileReader(recent));
			parser.parse(null, null, in);
		}
		catch(XmlException xe)
		{
			int line = xe.getLine();
			String message = xe.getMessage();
			Log.log(Log.ERROR,BufferHistory.class,recent + ":" + line
				+ ": " + message);
		}
		catch(FileNotFoundException fnf)
		{
			
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,BufferHistory.class,e);
		}
		finally
		{
			try
			{
				if(in != null)
					in.close();
			}
			catch(IOException io)
			{
				Log.log(Log.ERROR,BufferHistory.class,io);
			}
		}
	} 

	
	public static void save()
	{
		String settingsDirectory = jEdit.getSettingsDirectory();
		if(settingsDirectory == null)
			return;

		File file1 = new File(MiscUtilities.constructPath(
			settingsDirectory, "#recent.xml#save#"));
		File file2 = new File(MiscUtilities.constructPath(
			settingsDirectory, "recent.xml"));
		if(file2.exists() && file2.lastModified() != recentModTime)
		{
			Log.log(Log.WARNING,BufferHistory.class,file2
				+ " changed on disk; will not save recent"
				+ " files");
			return;
		}

		jEdit.backupSettingsFile(file2);

		Log.log(Log.MESSAGE,BufferHistory.class,"Saving " + file1);

		String lineSep = System.getProperty("line.separator");

		try
		{
			BufferedWriter out = new BufferedWriter(
				new FileWriter(file1));

			out.write("<?xml version=\"1.0\"?>");
			out.write(lineSep);
			out.write("<!DOCTYPE RECENT SYSTEM \"recent.dtd\">");
			out.write(lineSep);
			out.write("<RECENT>");
			out.write(lineSep);

			Iterator iter = history.iterator();
			while(iter.hasNext())
			{
				out.write("<ENTRY>");
				out.write(lineSep);

				Entry entry = (Entry)iter.next();

				out.write("<PATH>");
				out.write(MiscUtilities.charsToEntities(entry.path));
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

				out.write("</ENTRY>");
				out.write(lineSep);
			}

			out.write("</RECENT>");
			out.write(lineSep);

			out.close();

			
			file2.delete();
			file1.renameTo(file2);
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,BufferHistory.class,e);
		}

		recentModTime = file2.lastModified();
	} 

	
	private static LinkedList history;
	private static boolean pathsCaseInsensitive;
	private static long recentModTime;

	
	static
	{
		history = new LinkedList();
		pathsCaseInsensitive = OperatingSystem.isDOSDerived()
			|| OperatingSystem.isMacOS();
	} 

	
	 static void addEntry(Entry entry)
	{
		history.addFirst(entry);
		int max = jEdit.getIntegerProperty("recentFiles",50);
		while(history.size() > max)
			history.removeLast();
	} 

	
	 static void removeEntry(String path)
	{
		Iterator iter = history.iterator();
		while(iter.hasNext())
		{
			Entry entry = (Entry)iter.next();
			if(pathsCaseInsensitive)
			{
				if(entry.path.equalsIgnoreCase(path))
				{
					iter.remove();
					return;
				}
			}
			else
			{
				if(entry.path.equals(path))
				{
					iter.remove();
					return;
				}
			}
		}
	} 

	
	private static String selectionToString(Selection[] s)
	{
		if(s == null)
			return null;

		StringBuffer buf = new StringBuffer();

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

		Vector selection = new Vector();
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

			selection.addElement(sel);
		}

		Selection[] returnValue = new Selection[selection.size()];
		selection.copyInto(returnValue);
		return returnValue;
	} 

	

	
	
	public static class Entry
	{
		public String path;
		public int caret;
		public String selection;
		public String encoding;

		public Selection[] getSelection()
		{
			return stringToSelection(selection);
		}

		public Entry(String path, int caret, String selection, String encoding)
		{
			this.path = path;
			this.caret = caret;
			this.selection = selection;
			this.encoding = encoding;
		}
	} 

	
	static class RecentHandler extends HandlerBase
	{
		public void endDocument()
			throws java.lang.Exception
		{
			int max = jEdit.getIntegerProperty("recentFiles",50);
			while(history.size() > max)
				history.removeLast();
		}

		public Object resolveEntity(String publicId, String systemId)
		{
			if("recent.dtd".equals(systemId))
			{
				
				
				
				return new StringReader("<!-- -->");

				
			}

			return null;
		}

		public void doctypeDecl(String name, String publicId,
			String systemId) throws Exception
		{
			if("RECENT".equals(name))
				return;

			Log.log(Log.ERROR,this,"recent.xml: DOCTYPE must be RECENT");
		}

		public void endElement(String name)
		{
			if(name.equals("ENTRY"))
			{
				history.addLast(new Entry(
					path,caret,selection,
					encoding));
				path = null;
				caret = 0;
				selection = null;
				encoding = null;
			}
			else if(name.equals("PATH"))
				path = charData;
			else if(name.equals("CARET"))
				caret = Integer.parseInt(charData);
			else if(name.equals("SELECTION"))
				selection = charData;
			else if(name.equals("ENCODING"))
				encoding = charData;
		}

		public void charData(char[] ch, int start, int length)
		{
			charData = new String(ch,start,length);
		}

		

		
		private String path;
		private int caret;
		private String selection;
		private String encoding;
		private String charData;
	} 
}
