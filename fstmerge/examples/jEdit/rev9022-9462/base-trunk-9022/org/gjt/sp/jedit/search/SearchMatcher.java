

package org.gjt.sp.jedit.search;


public abstract class SearchMatcher
{
	public SearchMatcher()
	{
		returnValue = new Match();
	}

	
	public abstract Match nextMatch(CharSequence text, boolean start,
		boolean end, boolean firstTime, boolean reverse);

	
	public boolean isMatchingEOL()
	{
		return false;
	}

	protected Match returnValue;

	
	public static class Match
	{
		public int start;
		public int end;
		public String[] substitutions;
	} 
}
