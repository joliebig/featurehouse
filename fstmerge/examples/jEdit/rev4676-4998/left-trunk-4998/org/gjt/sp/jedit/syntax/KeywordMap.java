
package org.gjt.sp.jedit.syntax;

import javax.swing.text.Segment;
import java.util.Vector;


public class KeywordMap
{
	
	
	public KeywordMap(boolean ignoreCase)
	{
		this(ignoreCase, 52);
		this.ignoreCase = ignoreCase;
		noWordSep = new StringBuffer();
	} 

	
	
	public KeywordMap(boolean ignoreCase, int mapLength)
	{
		this.mapLength = mapLength;
		this.ignoreCase = ignoreCase;
		map = new Keyword[mapLength];
	} 

	
	
	public byte lookup(Segment text, int offset, int length)
	{
		if(length == 0)
			return Token.NULL;
		Keyword k = map[getSegmentMapKey(text, offset, length)];
		while(k != null)
		{
			if(length != k.keyword.length)
			{
				k = k.next;
				continue;
			}
			if(SyntaxUtilities.regionMatches(ignoreCase,text,offset,
				k.keyword))
				return k.id;
			k = k.next;
		}
		return Token.NULL;
	} 

	
	
	public void add(String keyword, byte id)
	{
		add(keyword.toCharArray(),id);
	} 

	
	
	public void add(char[] keyword, byte id)
	{
		int key = getStringMapKey(keyword);

		
		
loop:		for(int i = 0; i < keyword.length; i++)
		{
			char ch = keyword[i];
			if(!Character.isLetterOrDigit(ch))
			{
				for(int j = 0; j < noWordSep.length(); j++)
				{
					if(noWordSep.charAt(j) == ch)
						continue loop;
				}

				noWordSep.append(ch);
			}
		}

		noWordSepStr = null;
		map[key] = new Keyword(keyword,id,map[key]);
	} 

	
	
	public String getNonAlphaNumericChars()
	{
		return noWordSep.toString();
	} 

	
	
	public String[] getKeywords()
	{
		Vector vector = new Vector(100);
		for(int i = 0; i < map.length; i++)
		{
			Keyword keyword = map[i];
			while(keyword != null)
			{
				vector.addElement(new String(keyword.keyword));
				keyword = keyword.next;
			}
		}
		String[] retVal = new String[vector.size()];
		vector.copyInto(retVal);
		return retVal;
	} 

	
	
	public boolean getIgnoreCase()
	{
		return ignoreCase;
	} 

	
	
	public void setIgnoreCase(boolean ignoreCase)
	{
		this.ignoreCase = ignoreCase;
	} 

	
	
	public void add(KeywordMap map)
	{
		for(int i = 0; i < map.map.length; i++)
		{
			Keyword k = map.map[i];
			while(k != null)
			{
				add(k.keyword,k.id);
				k = k.next;
			}
		}
	} 

	

	
	private int mapLength;
	private Keyword[] map;
	private boolean ignoreCase;
	private StringBuffer noWordSep;
	private String noWordSepStr;
	

	
	private int getStringMapKey(String s)
	{
		return (Character.toUpperCase(s.charAt(0)) +
				Character.toUpperCase(s.charAt(s.length()-1)))
				% mapLength;
	} 

	
	private int getStringMapKey(char[] s)
	{
		return (Character.toUpperCase(s[0]) +
				Character.toUpperCase(s[s.length-1]))
				% mapLength;
	} 

	
	protected int getSegmentMapKey(Segment s, int off, int len)
	{
		return (Character.toUpperCase(s.array[off]) +
				Character.toUpperCase(s.array[off + len - 1]))
				% mapLength;
	} 

	

	
	class Keyword
	{
		public Keyword(char[] keyword, byte id, Keyword next)
		{
			this.keyword = keyword;
			this.id = id;
			this.next = next;
		}

		public char[] keyword;
		public byte id;
		public Keyword next;
	} 
}
