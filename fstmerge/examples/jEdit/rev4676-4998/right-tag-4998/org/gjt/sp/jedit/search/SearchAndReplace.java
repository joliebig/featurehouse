

package org.gjt.sp.jedit.search;


import bsh.BshMethod;
import java.awt.Component;
import javax.swing.JOptionPane;
import javax.swing.text.Segment;
import org.gjt.sp.jedit.*;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.msg.SearchSettingsChanged;
import org.gjt.sp.jedit.textarea.*;
import org.gjt.sp.util.CharIndexedSegment;
import org.gjt.sp.util.Log;



public class SearchAndReplace
{
	

	
	
	public static void setSearchString(String search)
	{
		if(!init)
			load();

		if(search.equals(SearchAndReplace.search))
			return;

		SearchAndReplace.search = search;
		matcher = null;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static String getSearchString()
	{
		if(!init)
			load();

		return search;
	} 

	
	
	public static void setReplaceString(String replace)
	{
		if(!init)
			load();

		if(replace.equals(SearchAndReplace.replace))
			return;

		SearchAndReplace.replace = replace;
		matcher = null;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static String getReplaceString()
	{
		if(!init)
			load();

		return replace;
	} 

	
	
	public static void setIgnoreCase(boolean ignoreCase)
	{
		if(!init)
			load();

		if(ignoreCase == SearchAndReplace.ignoreCase)
			return;

		SearchAndReplace.ignoreCase = ignoreCase;
		matcher = null;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static boolean getIgnoreCase()
	{
		if(!init)
			load();

		return ignoreCase;
	} 

	
	
	public static void setRegexp(boolean regexp)
	{
		if(!init)
			load();

		if(regexp == SearchAndReplace.regexp)
			return;

		SearchAndReplace.regexp = regexp;
		if(regexp && reverse)
			reverse = false;

		matcher = null;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static boolean getRegexp()
	{
		if(!init)
			load();

		return regexp;
	} 

	
	
	public static void setReverseSearch(boolean reverse)
	{
		if(!init)
			load();

		if(reverse == SearchAndReplace.reverse)
			return;

		SearchAndReplace.reverse = reverse;

		matcher = null;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static boolean getReverseSearch()
	{
		if(!init)
			load();

		return reverse;
	} 

	
	
	public static void setBeanShellReplace(boolean beanshell)
	{
		if(!init)
			load();

		if(beanshell == SearchAndReplace.beanshell)
			return;

		SearchAndReplace.beanshell = beanshell;
		matcher = null;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static boolean getBeanShellReplace()
	{
		if(!init)
			load();

		return beanshell;
	} 

	
	
	public static void setAutoWrapAround(boolean wrap)
	{
		if(!init)
			load();

		if(wrap == SearchAndReplace.wrap)
			return;

		SearchAndReplace.wrap = wrap;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static boolean getAutoWrapAround()
	{
		if(!init)
			load();

		return wrap;
	} 

	
	
	public static void setSearchMatcher(SearchMatcher matcher)
	{
		if(!init)
			load();

		SearchAndReplace.matcher = matcher;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static SearchMatcher getSearchMatcher()
		throws Exception
	{
		if(!init)
			load();

		if(matcher != null)
			return matcher;

		if(search == null || "".equals(search))
			return null;

		
		String replace = (SearchAndReplace.replace == null ? "" : SearchAndReplace.replace);

		BshMethod replaceMethod;
		if(beanshell && replace.length() != 0)
		{
			replaceMethod = BeanShell.cacheBlock("replace","return ("
				+ replace + ");",true);
		}
		else
			replaceMethod = null;

		if(regexp)
			matcher = new RESearchMatcher(search,replace,ignoreCase,
				beanshell,replaceMethod);
		else
		{
			matcher = new BoyerMooreSearchMatcher(search,replace,
				ignoreCase,beanshell,replaceMethod);
		}

		return matcher;
	} 

	
	
	public static void setSearchFileSet(SearchFileSet fileset)
	{
		if(!init)
			load();

		SearchAndReplace.fileset = fileset;

		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static SearchFileSet getSearchFileSet()
	{
		if(!init)
			load();

		return fileset;
	} 

	

	

	
	
	public static boolean hyperSearch(View view)
	{
		return hyperSearch(view,false);
	} 

	
	
	public static boolean hyperSearch(View view, boolean selection)
	{
		if(!init)
			load();

		
		Component comp = SearchDialog.getSearchDialog(view);
		if(comp == null)
			comp = view;

		record(view,"hyperSearch(view," + selection + ")",false,
			!selection);

		view.getDockableWindowManager().addDockableWindow(
			HyperSearchResults.NAME);
		final HyperSearchResults results = (HyperSearchResults)
			view.getDockableWindowManager()
			.getDockable(HyperSearchResults.NAME);
		results.searchStarted();

		try
		{
			SearchMatcher matcher = getSearchMatcher();
			if(matcher == null)
			{
				view.getToolkit().beep();
				results.searchFailed();
				return false;
			}

			Selection[] s;
			if(selection)
			{
				s = view.getTextArea().getSelection();
				if(s == null)
				{
					results.searchFailed();
					return false;
				}
			}
			else
				s = null;
			VFSManager.runInWorkThread(new HyperSearchRequest(view,
				matcher,results,s));
			return true;
		}
		catch(Exception e)
		{
			results.searchFailed();
			Log.log(Log.ERROR,SearchAndReplace.class,e);
			Object[] args = { e.getMessage() };
			if(args[0] == null)
				args[0] = e.toString();
			GUIUtilities.error(comp,"searcherror",args);
			return false;
		}
	} 

	
	
	public static boolean find(View view)
	{
		if(!init)
			load();

		
		Component comp = SearchDialog.getSearchDialog(view);
		if(comp == null)
			comp = view;

		boolean repeat = false;
		String path = fileset.getNextFile(view,null);
		if(path == null)
		{
			GUIUtilities.error(comp,"empty-fileset",null);
			return false;
		}

		boolean _reverse = reverse && fileset instanceof CurrentBufferSet;
		if(_reverse && regexp)
		{
			GUIUtilities.error(comp,"regexp-reverse",null);
			return false;
		}

		try
		{
			view.showWaitCursor();

			SearchMatcher matcher = getSearchMatcher();
			if(matcher == null)
			{
				view.getToolkit().beep();
				return false;
			}

			record(view,"find(view)",false,true);

loop:			for(;;)
			{
				while(path != null)
				{
					Buffer buffer = jEdit.openTemporary(
						view,null,path,false);

					
					path = fileset.getNextFile(view,path);

					if(buffer == null)
						continue loop;

					
					if(!buffer.isLoaded())
						VFSManager.waitForRequests();

					int start;

					if(view.getBuffer() == buffer && !repeat)
					{
						JEditTextArea textArea = view.getTextArea();
						Selection s = textArea.getSelectionAtOffset(
							textArea.getCaretPosition());
						if(s == null)
							start = textArea.getCaretPosition();
						else if(_reverse)
							start = s.getStart();
						else
							start = s.getEnd();
					}
					else if(_reverse)
						start = buffer.getLength();
					else
						start = 0;

					if(find(view,buffer,start,repeat,_reverse))
						return true;
				}

				if(repeat)
				{
					if(!BeanShell.isScriptRunning())
					{
						view.getStatus().setMessageAndClear(
							jEdit.getProperty("view.status.search-not-found"));

						view.getToolkit().beep();
					}
					return false;
				}

				boolean restart;

				if(BeanShell.isScriptRunning())
				{
					restart = true;
				}
				else if(wrap)
				{
					view.getStatus().setMessageAndClear(
						jEdit.getProperty("view.status.auto-wrap"));
					
					if(jEdit.getBooleanProperty("search.beepOnSearchAutoWrap"))
					{
						view.getToolkit().beep();
					}
					restart = true;
				}
				else
				{
					Integer[] args = { new Integer(_reverse ? 1 : 0) };
					int result = GUIUtilities.confirm(comp,
						"keepsearching",args,
						JOptionPane.YES_NO_OPTION,
						JOptionPane.QUESTION_MESSAGE);
					restart = (result == JOptionPane.YES_OPTION);
				}

				if(restart)
				{
					
					path = fileset.getFirstFile(view);
					repeat = true;
				}
				else
					break loop;
			}
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,SearchAndReplace.class,e);
			Object[] args = { e.getMessage() };
			if(args[0] == null)
				args[0] = e.toString();
			GUIUtilities.error(comp,"searcherror",args);
		}
		finally
		{
			view.hideWaitCursor();
		}

		return false;
	} 

	
	
	public static boolean find(View view, Buffer buffer, int start)
		throws Exception
	{
		return find(view,buffer,start,false,false);
	} 

	
	
	public static boolean find(View view, Buffer buffer, int start,
		boolean firstTime, boolean reverse) throws Exception
	{
		if(!init)
			load();

		SearchMatcher matcher = getSearchMatcher();
		if(matcher == null)
		{
			view.getToolkit().beep();
			return false;
		}

		Segment text = new Segment();
		if(reverse)
			buffer.getText(0,start,text);
		else
			buffer.getText(start,buffer.getLength() - start,text);

		
		
		
		
		
		int[] match = matcher.nextMatch(new CharIndexedSegment(text,reverse),
			start == 0,true,firstTime,reverse);

		if(match != null)
		{
			jEdit.commitTemporary(buffer);
			view.setBuffer(buffer);
			JEditTextArea textArea = view.getTextArea();

			if(reverse)
			{
				textArea.setSelection(new Selection.Range(
					start - match[1],
					start - match[0]));
				textArea.moveCaretPosition(start - match[1]);
			}
			else
			{
				textArea.setSelection(new Selection.Range(
					start + match[0],
					start + match[1]));
				textArea.moveCaretPosition(start + match[1]);
			}

			return true;
		}
		else
			return false;
	} 

	
	
	public static boolean replace(View view)
	{
		if(!init)
			load();

		
		Component comp = SearchDialog.getSearchDialog(view);
		if(comp == null)
			comp = view;

		JEditTextArea textArea = view.getTextArea();

		Buffer buffer = view.getBuffer();
		if(!buffer.isEditable())
			return false;

		boolean smartCaseReplace = (replace != null
			&& TextUtilities.getStringCase(replace)
			== TextUtilities.LOWER_CASE);

		Selection[] selection = textArea.getSelection();
		if(selection.length == 0)
		{
			view.getToolkit().beep();
			return false;
		}

		record(view,"replace(view)",true,false);

		
		int caret = textArea.getCaretPosition();
		Selection s = textArea.getSelectionAtOffset(caret);
		if(s != null)
			caret = s.getStart();

		try
		{
			buffer.beginCompoundEdit();

			SearchMatcher matcher = getSearchMatcher();
			if(matcher == null)
				return false;

			int retVal = 0;

			for(int i = 0; i < selection.length; i++)
			{
				s = selection[i];

				
				int start = s.getStart();

				if(s instanceof Selection.Range)
				{
					retVal += _replace(view,buffer,matcher,
						s.getStart(),s.getEnd(),
						smartCaseReplace);

					textArea.removeFromSelection(s);
					textArea.addToSelection(new Selection.Range(
						start,s.getEnd()));
				}
				else if(s instanceof Selection.Rect)
				{
					for(int j = s.getStartLine(); j <= s.getEndLine(); j++)
					{
						retVal += _replace(view,buffer,matcher,
							s.getStart(buffer,j),s.getEnd(buffer,j),
							smartCaseReplace);
					}
					textArea.addToSelection(new Selection.Rect(
						start,s.getEnd()));
				}
			}

			boolean _reverse = !regexp && reverse && fileset instanceof CurrentBufferSet;
			if(_reverse)
			{
				
				
				textArea.moveCaretPosition(caret);
			}
			else
			{
				s = textArea.getSelectionAtOffset(
					textArea.getCaretPosition());
				if(s != null)
					textArea.moveCaretPosition(s.getEnd());
			}

			if(retVal == 0)
			{
				view.getToolkit().beep();
				return false;
			}

			return true;
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,SearchAndReplace.class,e);
			Object[] args = { e.getMessage() };
			if(args[0] == null)
				args[0] = e.toString();
			GUIUtilities.error(comp,"searcherror",args);
		}
		finally
		{
			buffer.endCompoundEdit();
		}

		return false;
	} 

	
	
	public static boolean replace(View view, Buffer buffer, int start, int end)
	{
		if(!buffer.isEditable())
			return false;

		if(!init)
			load();

		
		Component comp = SearchDialog.getSearchDialog(view);
		if(comp == null)
			comp = view;

		boolean smartCaseReplace = (replace != null
			&& TextUtilities.getStringCase(replace)
			== TextUtilities.LOWER_CASE);

		try
		{
			buffer.beginCompoundEdit();

			SearchMatcher matcher = getSearchMatcher();
			if(matcher == null)
				return false;

			int retVal = 0;

			retVal += _replace(view,buffer,matcher,start,end,
				smartCaseReplace);

			if(retVal != 0)
				return true;
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,SearchAndReplace.class,e);
			Object[] args = { e.getMessage() };
			if(args[0] == null)
				args[0] = e.toString();
			GUIUtilities.error(comp,"searcherror",args);
		}
		finally
		{
			buffer.endCompoundEdit();
		}

		return false;
	} 

	
	
	public static boolean replaceAll(View view)
	{
		if(!init)
			load();

		
		Component comp = SearchDialog.getSearchDialog(view);
		if(comp == null)
			comp = view;

		int fileCount = 0;
		int occurCount = 0;

		if(fileset.getFileCount(view) == 0)
		{
			GUIUtilities.error(comp,"empty-fileset",null);
			return false;
		}

		record(view,"replaceAll(view)",true,true);

		view.showWaitCursor();

		boolean smartCaseReplace = (replace != null
			&& TextUtilities.getStringCase(replace)
			== TextUtilities.LOWER_CASE);

		try
		{
			SearchMatcher matcher = getSearchMatcher();
			if(matcher == null)
				return false;

			String path = fileset.getFirstFile(view);
loop:			while(path != null)
			{
				Buffer buffer = jEdit.openTemporary(
					view,null,path,false);

				
				path = fileset.getNextFile(view,path);

				if(buffer == null)
					continue loop;

				
				if(buffer.isPerformingIO())
					VFSManager.waitForRequests();

				if(!buffer.isEditable())
					continue loop;

				
				
				int retVal = 0;

				try
				{
					buffer.beginCompoundEdit();
					retVal = _replace(view,buffer,matcher,
						0,buffer.getLength(),
						smartCaseReplace);
				}
				finally
				{
					buffer.endCompoundEdit();
				}

				if(retVal != 0)
				{
					fileCount++;
					occurCount += retVal;
					jEdit.commitTemporary(buffer);
				}
			}
		}
		catch(Exception e)
		{
			Log.log(Log.ERROR,SearchAndReplace.class,e);
			Object[] args = { e.getMessage() };
			if(args[0] == null)
				args[0] = e.toString();
			GUIUtilities.error(comp,"searcherror",args);
		}
		finally
		{
			view.hideWaitCursor();
		}

		
		if(!BeanShell.isScriptRunning())
		{
			Object[] args = { new Integer(occurCount),
				new Integer(fileCount) };
			view.getStatus().setMessageAndClear(jEdit.getProperty(
				"view.status.replace-all",args));
			if(occurCount == 0)
				view.getToolkit().beep();
		}

		return (fileCount != 0);
	} 

	

	
	
	public static void load()
	{
		init = true;
		search = jEdit.getProperty("search.find.value");
		replace = jEdit.getProperty("search.replace.value");
		ignoreCase = jEdit.getBooleanProperty("search.ignoreCase.toggle");
		regexp = jEdit.getBooleanProperty("search.regexp.toggle");
		beanshell = jEdit.getBooleanProperty("search.beanshell.toggle");
		wrap = jEdit.getBooleanProperty("search.wrap.toggle");

		fileset = new CurrentBufferSet();

		
		
		
		matcher = null;
		EditBus.send(new SearchSettingsChanged(null));
	} 

	
	
	public static void save()
	{
		if(!init)
			return;

		jEdit.setProperty("search.find.value",search);
		jEdit.setProperty("search.replace.value",replace);
		jEdit.setBooleanProperty("search.ignoreCase.toggle",ignoreCase);
		jEdit.setBooleanProperty("search.regexp.toggle",regexp);
		jEdit.setBooleanProperty("search.beanshell.toggle",beanshell);
		jEdit.setBooleanProperty("search.wrap.toggle",wrap);
	} 

	

	
	private static boolean init;

	private static String search;
	private static String replace;
	private static boolean regexp;
	private static boolean ignoreCase;
	private static boolean reverse;
	private static boolean beanshell;
	private static boolean wrap;
	private static SearchMatcher matcher;
	private static SearchFileSet fileset;
	

	
	private static void record(View view, String action,
		boolean replaceAction, boolean recordFileSet)
	{
		Macros.Recorder recorder = view.getMacroRecorder();

		if(recorder != null)
		{
			recorder.record("SearchAndReplace.setSearchString(\""
				+ MiscUtilities.charsToEscapes(search) + "\");");

			if(replaceAction)
			{
				recorder.record("SearchAndReplace.setReplaceString(\""
					+ MiscUtilities.charsToEscapes(replace) + "\");");
				recorder.record("SearchAndReplace.setBeanShellReplace("
					+ beanshell + ");");
			}
			else
			{
				
				recorder.record("SearchAndReplace.setAutoWrapAround("
					+ wrap + ");");
				recorder.record("SearchAndReplace.setReverseSearch("
					+ reverse + ");");
			}

			recorder.record("SearchAndReplace.setIgnoreCase("
				+ ignoreCase + ");");
			recorder.record("SearchAndReplace.setRegexp("
				+ regexp + ");");

			if(recordFileSet)
			{
				recorder.record("SearchAndReplace.setSearchFileSet("
					+ fileset.getCode() + ");");
			}

			recorder.record("SearchAndReplace." + action + ";");
		}
	} 

	
	
	private static int _replace(View view, Buffer buffer,
		SearchMatcher matcher, int start, int end,
		boolean smartCaseReplace)
		throws Exception
	{
		int occurCount = 0;

		boolean endOfLine = (buffer.getLineEndOffset(
			buffer.getLineOfOffset(end)) - 1 == end);

		Segment text = new Segment();
		int offset = start;
loop:		for(int counter = 0; ; counter++)
		{
			buffer.getText(offset,end - offset,text);

			boolean startOfLine = (buffer.getLineStartOffset(
				buffer.getLineOfOffset(offset)) == offset);

			int[] occur = matcher.nextMatch(
				new CharIndexedSegment(text,false),
				startOfLine,endOfLine,counter == 0,
				false);
			if(occur == null)
				break loop;
			int _start = occur[0];
			int _length = occur[1] - occur[0];

			String found = new String(text.array,text.offset + _start,_length);
			String subst = matcher.substitute(found);
			if(smartCaseReplace && ignoreCase)
			{
				int strCase = TextUtilities.getStringCase(found);
				if(strCase == TextUtilities.LOWER_CASE)
					subst = subst.toLowerCase();
				else if(strCase == TextUtilities.UPPER_CASE)
					subst = subst.toUpperCase();
				else if(strCase == TextUtilities.TITLE_CASE)
					subst = TextUtilities.toTitleCase(subst);
			}

			if(subst != null)
			{
				buffer.remove(offset + _start,_length);
				buffer.insert(offset + _start,subst);
				occurCount++;
				offset += _start + subst.length();

				end += (subst.length() - found.length());
			}
			else
				offset += _start + _length;
		}

		return occurCount;
	} 

	
}
