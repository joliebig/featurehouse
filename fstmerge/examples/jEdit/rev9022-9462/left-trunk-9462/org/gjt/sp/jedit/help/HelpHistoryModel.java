

package org.gjt.sp.jedit.help;

import java.net.URL;


public class HelpHistoryModel
{
	
	public HelpHistoryModel(int size)
	{
		int historyPos = 0;
		history = new HistoryEntry[size];
		listeners = new java.util.Vector();
	} 

	
	public HistoryEntry forward(HelpViewer helpViewer)
	{
		if(history.length - historyPos <= 1)
		{
			return null;
		}
		if (history[historyPos] == null)
		{
			return null;
		}
		setCurrentScrollPosition(helpViewer.getCurrentPage(),helpViewer.getCurrentScrollPosition());
		HistoryEntry result = new HistoryEntry(history[historyPos]);
		historyPos++;
		fireUpdate();
		return result;
	} 

	
	public boolean hasNext()
	{
		return !((history.length - historyPos <= 1) ||
			 (history[historyPos] == null));
	} 

	
	public HistoryEntry back(HelpViewer helpViewer)
	{
		if (historyPos <= 1)
		{
			return null;
		}
		setCurrentScrollPosition(helpViewer.getCurrentPage(),helpViewer.getCurrentScrollPosition());
		HistoryEntry result = new HistoryEntry(history[--historyPos - 1]);
		fireUpdate();
		return result;
	} 

	
	public boolean hasPrevious()
	{
		return (historyPos>1);
	} 

	
	public void addToHistory(String url)
	{
		history[historyPos] = new HistoryEntry(url,url,0);
		if(historyPos + 1 == history.length)
		{
			System.arraycopy(history,1,history,
					 0,history.length - 1);
			history[historyPos] = null;
		}
		else
		{
			historyPos++;
			for (int i = historyPos ; i<history.length ; i++)
			{
				history[i] = null;
			}
		}
		fireUpdate();
	} 

	
	public void setCurrentScrollPosition(URL currentPage, int scrollPosition)
	{
		if ((null != currentPage) && (historyPos >= 1) &&
		    (currentPage.toString().equals(history[historyPos-1].url)))
		{
			history[historyPos-1].scrollPosition = scrollPosition;
		}
	} 

	
	public void setCurrentEntry(HistoryEntry entry)
	{
		for (int i=0 ; i<history.length ; i++)
		{
			if ((history[i] != null) && (history[i].equals(entry)))
			{
				historyPos = i+1;
				fireUpdate();
				break;
			}
		}
		
	} 

	
	public void updateTitle(String url, String title)
	{
		for (int i=0;i<history.length;i++)
		{
			if ((history[i] != null) && history[i].url.equals(url))
			{
				history[i].title = title;
			}
		}
		fireUpdate();
	}

	
	public HistoryEntry[] getPreviousURLs()
	{
		if (historyPos<=1)
		{
			return new HelpHistoryModel.HistoryEntry[0];
		}
		HistoryEntry[] previous = new HistoryEntry[historyPos-1];
		System.arraycopy(history,0,previous,0,historyPos-1);
		return previous;
	} 

	
	public HistoryEntry[] getNextURLs()
	{
		if (history.length - historyPos <= 1)
		{
			return new HelpHistoryModel.HistoryEntry[0];
		}
		if (history[historyPos] == null)
		{
			return new HelpHistoryModel.HistoryEntry[0];
		}
		HistoryEntry[] next = new HistoryEntry[history.length-historyPos];
		System.arraycopy(history,historyPos,next,0,history.length-historyPos);
		return next;
	} 

	
	public void addHelpHistoryModelListener(HelpHistoryModelListener hhml)
	{
		listeners.add(hhml);
	} 

	
	public void removeHelpHistoryModelListener(HelpHistoryModelListener hhml)
	{
		listeners.remove(hhml);
	} 

	
	public void fireUpdate()
	{
		for (int i=0 ; i<listeners.size() ; i++)
		{
			((HelpHistoryModelListener)listeners.elementAt(i)).historyUpdated();
		}
	} 

	
	private int historyPos;
	private HistoryEntry[] history;
	private java.util.Vector listeners;
	

	

	
	static class HistoryEntry
	{
		String url;
		String title;
		int scrollPosition;

		
		HistoryEntry(String url, String title)
		{
			this(url,title,0);
		} 

		
		HistoryEntry(HistoryEntry original)
		{
			this(original.url,original.title,original.scrollPosition);
		} 

		
		HistoryEntry(String url, String title, int scrollPosition)
		{
			this.url = url;
			this.title = title;
			this.scrollPosition = scrollPosition;
		} 

		
		public boolean equals(HistoryEntry he)
		{
			return he.url.equals(this.url) &&
			       he.title.equals(this.title) &&
			       (he.scrollPosition == scrollPosition);
		} 

		
		public String toString()
		{
			return getClass().getName() + "[url=" + url + ",title=" + title
			+ ",scrollPosition=" + scrollPosition + "]";
		} 
	} 

	
}
