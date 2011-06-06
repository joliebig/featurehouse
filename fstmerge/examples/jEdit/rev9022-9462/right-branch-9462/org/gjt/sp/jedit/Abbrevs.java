

package org.gjt.sp.jedit;


import java.io.*;
import java.util.*;
import org.gjt.sp.jedit.gui.AddAbbrevDialog;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.Log;



public class Abbrevs
{
	public static final String ENCODING = "UTF8";

	
	
	public static boolean getExpandOnInput()
	{
		return expandOnInput;
	} 

	
	
	public static void setExpandOnInput(boolean expandOnInput)
	{
		Abbrevs.expandOnInput = expandOnInput;
	} 

	
	
	public static boolean expandAbbrev(View view, boolean add)
	{
		
		Buffer buffer = view.getBuffer();
		JEditTextArea textArea = view.getTextArea();
		if(!buffer.isEditable())
		{
			view.getToolkit().beep();
			return false;
		}

		int line = textArea.getCaretLine();
		int lineStart = buffer.getLineStartOffset(line);
		int caret = textArea.getCaretPosition();

		String lineText = buffer.getLineText(line);
		if(lineText.length() == 0)
		{
			if(add)
				view.getToolkit().beep();
			return false;
		}

		int pos = caret - lineStart;
		if(pos == 0)
		{
			if(add)
				view.getToolkit().beep();
			return false;
		} 

		
		m_pp.removeAllElements();

		int wordStart;
		String abbrev;

		
		if(lineText.charAt(pos-1) == '#')
		{
			wordStart = lineText.indexOf('#');
			wordStart = TextUtilities.findWordStart(lineText,wordStart,
				buffer.getStringProperty("noWordSep") + '#');

			abbrev = lineText.substring(wordStart,pos - 1);

			
			

			int lastIndex = 0;
			for(int i = 0; i < abbrev.length(); i++)
			{
				if(abbrev.charAt(i) == '#')
				{
					m_pp.addElement(abbrev.substring(lastIndex,i));
					lastIndex = i + 1;
				}
			}

			m_pp.addElement(abbrev.substring(lastIndex));

			
			abbrev = (String)m_pp.elementAt(0);
			m_pp.removeElementAt(0);
		} 
		
		else
		{
			wordStart = TextUtilities.findWordStart(lineText,pos - 1,
				buffer.getStringProperty("noWordSep"));

			abbrev = lineText.substring(wordStart,pos);
		} 

		Expansion expand = expandAbbrev(buffer.getMode().getName(),
			abbrev,(buffer.getBooleanProperty("noTabs") ?
			buffer.getTabSize() : 0),m_pp);

		
		if(expand == null)
		{
			if(add)
				new AddAbbrevDialog(view,abbrev);

			return false;
		} 
		
		else
		{
			buffer.remove(lineStart + wordStart,
				pos - wordStart);

			int whitespace = buffer.insertIndented(
				lineStart + wordStart,
				expand.text);

			int newlines = countNewlines(expand.text,
				expand.caretPosition);

			if(expand.caretPosition != -1)
			{
				textArea.setCaretPosition(lineStart + wordStart
					+ expand.caretPosition
					+ newlines * whitespace);
			}
			if(expand.posParamCount != m_pp.size())
			{
				view.getStatus().setMessageAndClear(
					jEdit.getProperty(
					"view.status.incomplete-abbrev",
					new Integer[] { new Integer(m_pp.size()),
					new Integer(expand.posParamCount) }));
			}

			return true;
		} 
	} 

	
	
	public static Hashtable getGlobalAbbrevs()
	{
		if(!loaded)
			load();

		return globalAbbrevs;
	} 

	
	
	public static void setGlobalAbbrevs(Hashtable globalAbbrevs)
	{
		abbrevsChanged = true;
		Abbrevs.globalAbbrevs = globalAbbrevs;
	} 

	
	
	public static Hashtable getModeAbbrevs()
	{
		if(!loaded)
			load();

		return modes;
	} 

	
	
	public static void setModeAbbrevs(Hashtable modes)
	{
		abbrevsChanged = true;
		Abbrevs.modes = modes;
	} 

	
	
	public static void addGlobalAbbrev(String abbrev, String expansion)
	{
		if(!loaded)
			load();

		globalAbbrevs.put(abbrev,expansion);
		abbrevsChanged = true;
	} 

	
	
	public static void addModeAbbrev(String mode, String abbrev, String expansion)
	{
		if(!loaded)
			load();

		Hashtable modeAbbrevs = (Hashtable)modes.get(mode);
		if(modeAbbrevs == null)
		{
			modeAbbrevs = new Hashtable();
			modes.put(mode,modeAbbrevs);
		}
		modeAbbrevs.put(abbrev,expansion);
		abbrevsChanged = true;
	} 

	
	static void save()
	{
		jEdit.setBooleanProperty("view.expandOnInput",expandOnInput);

		String settings = jEdit.getSettingsDirectory();
		if(abbrevsChanged && settings != null)
		{
			File file1 = new File(MiscUtilities.constructPath(settings,"#abbrevs#save#"));
			File file2 = new File(MiscUtilities.constructPath(settings,"abbrevs"));
			if(file2.exists() && file2.lastModified() != abbrevsModTime)
			{
				Log.log(Log.WARNING,Abbrevs.class,file2 + " changed on disk;"
					+ " will not save abbrevs");
			}
			else
			{
				jEdit.backupSettingsFile(file2);

				try
				{
					saveAbbrevs(new OutputStreamWriter(
						new FileOutputStream(file1),
						ENCODING));
					file2.delete();
					file1.renameTo(file2);
				}
				catch(Exception e)
				{
					Log.log(Log.ERROR,Abbrevs.class,"Error while saving " + file1);
					Log.log(Log.ERROR,Abbrevs.class,e);
				}
				abbrevsModTime = file2.lastModified();
			}
		}
	} 

	

	
	private static boolean loaded;
	private static boolean abbrevsChanged;
	private static long abbrevsModTime;
	private static boolean expandOnInput;
	private static Hashtable globalAbbrevs;
	private static Hashtable modes;
	
	
	private static Vector m_pp = new Vector();
	

	private Abbrevs() {}

	static
	{
		expandOnInput = jEdit.getBooleanProperty("view.expandOnInput");
	}

	
	private static void load()
	{
		globalAbbrevs = new Hashtable();
		modes = new Hashtable();

		String settings = jEdit.getSettingsDirectory();
		if(settings != null)
		{
			File file = new File(MiscUtilities.constructPath(settings,"abbrevs"));
			abbrevsModTime = file.lastModified();

			try
			{
				loadAbbrevs(new InputStreamReader(
					new FileInputStream(file),ENCODING));
				loaded = true;
			}
			catch(FileNotFoundException fnf)
			{
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,Abbrevs.class,"Error while loading " + file);
				Log.log(Log.ERROR,Abbrevs.class,e);
			}
		}

		
		if(!loaded)
		{
			try
			{
				loadAbbrevs(new InputStreamReader(Abbrevs.class
					.getResourceAsStream("default.abbrevs"),
					ENCODING));
			}
			catch(Exception e)
			{
				Log.log(Log.ERROR,Abbrevs.class,"Error while loading default.abbrevs");
				Log.log(Log.ERROR,Abbrevs.class,e);
			}
			loaded = true;
		}
	} 

	
	private static int countNewlines(String s, int end)
	{
		int counter = 0;

		for(int i = 0; i < end; i++)
		{
			if(s.charAt(i) == '\n')
				counter++;
		}

		return counter;
	} 

	
	private static Expansion expandAbbrev(String mode, String abbrev,
		int softTabSize, Vector pp)
	{
		m_pp = pp;
		if(!loaded)
			load();

		
		String expand = null;
		Hashtable modeAbbrevs = (Hashtable)modes.get(mode);
		if(modeAbbrevs != null)
			expand = (String)modeAbbrevs.get(abbrev);

		if(expand == null)
			expand = (String)globalAbbrevs.get(abbrev);

		if(expand == null)
			return null;
		else
			return new Expansion(expand,softTabSize,m_pp);
	} 

	
	private static void loadAbbrevs(Reader _in) throws Exception
	{
		BufferedReader in = new BufferedReader(_in);

		try
		{
			Hashtable currentAbbrevs = globalAbbrevs;

			String line;
			while((line = in.readLine()) != null)
			{
				int index = line.indexOf('|');

				if(line.length() == 0)
					continue;
				else if(line.startsWith("[") && index == -1)
				{
					if(line.equals("[global]"))
						currentAbbrevs = globalAbbrevs;
					else
					{
						String mode = line.substring(1,
							line.length() - 1);
						currentAbbrevs = (Hashtable)modes.get(mode);
						if(currentAbbrevs == null)
						{
							currentAbbrevs = new Hashtable();
							modes.put(mode,currentAbbrevs);
						}
					}
				}
				else if(index != -1)
				{
					currentAbbrevs.put(line.substring(0,index),
						line.substring(index + 1));
				}
			}
		}
		finally
		{
			in.close();
		}
	} 

	
	private static void saveAbbrevs(Writer _out) throws Exception
	{
		BufferedWriter out = new BufferedWriter(_out);
		String lineSep = System.getProperty("line.separator");

		
		out.write("[global]");
		out.write(lineSep);

		saveAbbrevs(out,globalAbbrevs);

		
		Enumeration keys = modes.keys();
		Enumeration values = modes.elements();
		while(keys.hasMoreElements())
		{
			out.write('[');
			out.write((String)keys.nextElement());
			out.write(']');
			out.write(lineSep);
			saveAbbrevs(out,(Hashtable)values.nextElement());
		}

		out.close();
	} 

	
	private static void saveAbbrevs(Writer out, Hashtable abbrevs)
		throws Exception
	{
		String lineSep = System.getProperty("line.separator");

		Enumeration keys = abbrevs.keys();
		Enumeration values = abbrevs.elements();
		while(keys.hasMoreElements())
		{
			String abbrev = (String)keys.nextElement();
			out.write(abbrev);
			out.write('|');
			out.write(values.nextElement().toString());
			out.write(lineSep);
		}
	} 

	

	
	static class Expansion
	{
		String text;
		int caretPosition = -1;
		int lineCount;

		
		int posParamCount;

		
		Expansion(String text, int softTabSize, Vector pp)
		{
			StringBuffer buf = new StringBuffer();
			boolean backslash = false;

			for(int i = 0; i < text.length(); i++)
			{
				char ch = text.charAt(i);
				
				if(backslash)
				{
					backslash = false;

					if(ch == '|')
						caretPosition = buf.length();
					else if(ch == 'n')
					{
						buf.append('\n');
						lineCount++;
					}
					else if(ch == 't')
					{
						if(softTabSize == 0)
							buf.append('\t');
						else
						{
							for(int j = 0; j < softTabSize; j++)
								buf.append(' ');
						}
					}
					else
						buf.append(ch);
				}
				else if(ch == '\\')
					backslash = true;
				
				
				else if(ch == '$')
				{
					if(i != text.length() - 1)
					{
						ch = text.charAt(i + 1);
						if(Character.isDigit(ch) && ch != '0')
						{
							i++;

							int pos = ch - '0';
							posParamCount = Math.max(pos,posParamCount);
							
							
							if(pos <= pp.size())
								buf.append(pp.elementAt(pos - 1));
						}
						else
						{
							
							
							buf.append('$');
						}
					}
					else
						buf.append('$'); 
				} 
				else
					buf.append(ch);
			}

			this.text = buf.toString();
		} 
	} 
}
