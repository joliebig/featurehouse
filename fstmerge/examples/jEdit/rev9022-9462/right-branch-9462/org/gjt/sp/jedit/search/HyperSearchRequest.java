

package org.gjt.sp.jedit.search;


import javax.swing.text.Segment;
import javax.swing.tree.*;
import javax.swing.SwingUtilities;
import org.gjt.sp.jedit.textarea.Selection;
import org.gjt.sp.jedit.io.VFSManager;
import org.gjt.sp.jedit.Buffer;
import org.gjt.sp.jedit.GUIUtilities;
import org.gjt.sp.jedit.jEdit;
import org.gjt.sp.jedit.View;
import org.gjt.sp.util.*;



class HyperSearchRequest extends WorkRequest
{
	
	HyperSearchRequest(View view, SearchMatcher matcher,
		HyperSearchResults results, Selection[] selection)
	{
		this.view = view;
		this.matcher = matcher;

		this.results = results;
		searchString = SearchAndReplace.getSearchString();
		rootSearchNode = new DefaultMutableTreeNode(new HyperSearchOperationNode(searchString));

		this.selection = selection;
	} 

	
	public void run()
	{
		setStatus(jEdit.getProperty("hypersearch-status"));

		SearchFileSet fileset = SearchAndReplace.getSearchFileSet();
		String[] files = fileset.getFiles(view);
		if(files == null || files.length == 0)
		{
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					GUIUtilities.error(view,"empty-fileset",null);
					results.searchDone(rootSearchNode);
				}
			});
			return;
		}

		setMaximum(fileset.getFileCount(view));

		
		

		
		String searchingCaption = jEdit.getProperty("hypersearch-results.searching") + ' ';
		try
		{
			if(selection != null)
			{
				Buffer buffer = view.getBuffer();

				searchInSelection(buffer);
			}
			else
			{
				int current = 0;

				long lastStatusTime = 0;
loop:				for(int i = 0; i < files.length; i++)
				{
					String file = files[i];
					current++;

					long currentTime = System.currentTimeMillis();
					if(currentTime - lastStatusTime > 250)
					{
						setValue(current);
						lastStatusTime = currentTime;
						results.setSearchStatus(searchingCaption + file);
					}

					Buffer buffer = jEdit.openTemporary(null,null,file,false);
					if(buffer == null)
						continue loop;

					doHyperSearch(buffer);
				}
			}
		}
		catch(final Exception e)
		{
			Log.log(Log.ERROR,this,e);
			SwingUtilities.invokeLater(new Runnable()
			{
				public void run()
				{
					SearchAndReplace.handleError(view,e);
				}
			});
		}
		catch(WorkThread.Abort a)
		{
		}
		finally
		{
			VFSManager.runInAWTThread(new Runnable()
			{
				public void run()
				{
					results.searchDone(rootSearchNode);
				}
			});
		}
	} 

	

	
	private View view;
	private SearchMatcher matcher;
	private HyperSearchResults results;
	private DefaultMutableTreeNode rootSearchNode;
	private Selection[] selection;
	private String searchString;
	

	
	private int searchInSelection(Buffer buffer) throws Exception
	{
		setAbortable(false);

		int resultCount = 0;

		try
		{
			buffer.readLock();

			for(int i = 0; i < selection.length; i++)
			{
				Selection s = selection[i];
				if(s instanceof Selection.Rect)
				{
					for(int j = s.getStartLine();
						j <= s.getEndLine(); j++)
					{
						resultCount += doHyperSearch(buffer,
							s.getStart(buffer,j),
							s.getEnd(buffer,j));
					}
				}
				else
				{
					resultCount += doHyperSearch(buffer,
						s.getStart(),s.getEnd());
				}
			}
		}
		finally
		{
			buffer.readUnlock();
		}

		setAbortable(true);

		return resultCount;
	} 

	
	private int doHyperSearch(Buffer buffer)
		throws Exception
	{
		return doHyperSearch(buffer, 0, buffer.getLength());
	} 

	
	private int doHyperSearch(Buffer buffer, int start, int end)
		throws Exception
	{
		setAbortable(false);

		HyperSearchFileNode hyperSearchFileNode = new HyperSearchFileNode(buffer.getPath());
		DefaultMutableTreeNode bufferNode = new DefaultMutableTreeNode(hyperSearchFileNode);

		int resultCount = doHyperSearch(buffer,start,end,bufferNode);
		hyperSearchFileNode.setCount(resultCount);
		if(resultCount != 0)
			rootSearchNode.insert(bufferNode,rootSearchNode.getChildCount());

		setAbortable(true);

		return resultCount;
	} 

	
	private int doHyperSearch(Buffer buffer, int start, int end,
		DefaultMutableTreeNode bufferNode)
	{
		int resultCount = 0;

		try
		{
			buffer.readLock();

			boolean endOfLine = buffer.getLineEndOffset(
				buffer.getLineOfOffset(end)) - 1 == end;

			int offset = start;

			HyperSearchResult lastResult = null;

loop:			for(int counter = 0; ; counter++)
			{
				boolean startOfLine = buffer.getLineStartOffset(
					buffer.getLineOfOffset(offset)) == offset;

				CharSequence segment = buffer.getSegment(offset,end - offset);
				SearchMatcher.Match match = matcher.nextMatch(
					segment, startOfLine, endOfLine,
					counter == 0, false);
				if(match == null)
					break loop;

				int newLine = buffer.getLineOfOffset(
					offset + match.start);
				if(lastResult == null || lastResult.line != newLine)
				{
					lastResult = new HyperSearchResult(
						buffer,newLine);
					bufferNode.add(
						new DefaultMutableTreeNode(
						lastResult,false));
				}

				lastResult.addOccur(offset + match.start,
					offset + match.end);

				offset += match.end;
				if (matcher.isMatchingEOL())
				{
					if (offset < buffer.getLength())
						offset += 1;
					else
						break loop;
				}

				resultCount++;
			}
		}
		finally
		{
			buffer.readUnlock();
		}

		return resultCount;
	} 

	
}
